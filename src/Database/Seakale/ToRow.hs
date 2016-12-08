{-# LANGUAGE OverloadedLists #-}

module Database.Seakale.ToRow
  ( ToRow(..)
  ) where

import           GHC.Generics
import           GHC.Int

import           Data.Monoid
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Data.Text.Lazy        as TL

import           Database.Seakale.Types

class ToRow backend n a | a -> n where
  toRow :: backend -> a -> QueryData n

  default toRow :: (Generic a, GToRow backend WithCon n (Rep a))
                => backend -> a -> QueryData n
  toRow backend =
    fst . gtoRow backend WithCon . toValueProxy backend WithCon . Just . from

data WithCon    = WithCon
data WithoutCon = WithoutCon

class GToRow backend con n f | f -> n where
  data ValueProxy con f a
  toValueProxy :: backend -> con -> Maybe (f a) -> ValueProxy con f a
  gtoRow :: backend -> con -> ValueProxy con f a -> (QueryData n, Maybe String)

instance GToRow backend con Zero V1 where
  data ValueProxy con V1 a = ProxyV1
  toValueProxy _ _ _ = ProxyV1
  gtoRow _ _ _ = (Nil, Nothing)

instance GToRow backend con Zero U1 where
  data ValueProxy con U1 a = ProxyU1
  toValueProxy _ _ _ = ProxyU1
  gtoRow _ _ _ = (Nil, Nothing)

instance (GToRow backend con n a, GToRow backend con m b, (n :+ m) ~ i)
  => GToRow backend con i (a :*: b) where

  data ValueProxy con (a :*: b) c
    = ProxyProduct (ValueProxy con a c) (ValueProxy con b c)

  toValueProxy backend con = \case
    Just (x :*: y) -> ProxyProduct (toValueProxy backend con (Just x))
                                   (toValueProxy backend con (Just y))
    Nothing        -> ProxyProduct (toValueProxy backend con Nothing)
                                   (toValueProxy backend con Nothing)

  gtoRow backend con (ProxyProduct a b) =
    let vec = fst (gtoRow backend con a) `vappend` fst (gtoRow backend con b)
    in (vec, Nothing)

instance ( GToRow backend WithoutCon n a, GToRow backend WithoutCon m b
         , (n :+ m) ~ i ) => GToRow backend WithoutCon i (a :+: b) where

  data ValueProxy WithoutCon (a :+: b) c
    = ProxySumNone (ValueProxy WithoutCon a c) (ValueProxy WithoutCon b c)
    | ProxySumLeft (a c) (ValueProxy WithoutCon b c)
    | ProxySumRight (ValueProxy WithoutCon a c) (b c)

  toValueProxy backend con = \case
    Just (L1 x) -> ProxySumLeft x (toValueProxy backend con Nothing)
    Just (R1 x) -> ProxySumRight (toValueProxy backend con Nothing) x
    Nothing     -> ProxySumNone (toValueProxy backend con Nothing)
                                (toValueProxy backend con Nothing)

  gtoRow backend con = \case
    ProxySumNone proxyA proxyB ->
      let vec = fst (gtoRow backend con proxyA)
                `vappend` fst (gtoRow backend con proxyB)
      in (vec, Nothing)
    ProxySumLeft a proxyB ->
      let (vecA, mConName) =
            gtoRow backend con (toValueProxy backend con (Just a))
          vec = vecA `vappend` fst (gtoRow backend con proxyB)
      in (vec, mConName)
    ProxySumRight proxyA b ->
      let (vecB, mConName) =
            gtoRow backend con (toValueProxy backend con (Just b))
          vec = fst (gtoRow backend con proxyA) `vappend` vecB
      in (vec, mConName)

instance ( GToRow backend WithoutCon n a, GToRow backend WithoutCon m b
         , 'S (n :+ m) ~ i ) => GToRow backend WithCon i (a :+: b) where

  data ValueProxy WithCon (a :+: b) c
    = ProxySumCon (ValueProxy WithoutCon (a :+: b) c)

  toValueProxy backend _ = ProxySumCon . toValueProxy backend WithoutCon

  gtoRow backend WithCon (ProxySumCon proxy) =
    case gtoRow backend WithoutCon proxy of
      (_, Nothing) -> error "GToRow _ WithCon _ (_ :+: _): no constructor name"
      (vec, Just name) -> (Cons (formatString (BS.pack name)) vec, Nothing)

instance (Backend backend, NTimes (Vector n), ToRow backend n a)
  => GToRow backend con n (K1 i a) where
  data ValueProxy con (K1 i a) b = ProxyConst (Maybe a)
  toValueProxy _ _ = ProxyConst . fmap unK1
  gtoRow backend _ = (,Nothing) . \case
    ProxyConst Nothing  -> ntimes "NULL"
    ProxyConst (Just x) -> toRow backend x

instance (Constructor c, GToRow backend con n a)
  => GToRow backend con n (M1 C c a) where

  data ValueProxy con (M1 C c a) b
    = ProxyMetaC (ValueProxy con a b) (Maybe String)

  toValueProxy backend con mM1 =
    ProxyMetaC (toValueProxy backend con (fmap unM1 mM1))
               (fmap conName mM1)

  gtoRow backend con (ProxyMetaC vp mConName) =
    let vec = fst $ gtoRow backend con vp
    in (vec, mConName)

instance GToRow backend con n a => GToRow backend con n (M1 D c a) where
  data ValueProxy con(M1 D c a) b = ProxyMetaD (ValueProxy con a b)
  toValueProxy backend con = ProxyMetaD . toValueProxy backend con . fmap unM1
  gtoRow backend con (ProxyMetaD a) = gtoRow backend con a

instance GToRow backend con n a => GToRow backend con n (M1 S c a) where
  data ValueProxy con (M1 S c a) b = ProxyMetaS (ValueProxy con a b)
  toValueProxy backend con = ProxyMetaS . toValueProxy backend con . fmap unM1
  gtoRow backend con (ProxyMetaS a) = gtoRow backend con a

instance ToRow backend Zero ()

formatString :: BS.ByteString -> BS.ByteString
formatString str = "'" <> escapeQuotes str <> "'"
  where
    escapeQuotes :: BS.ByteString -> BS.ByteString
    escapeQuotes "" = ""
    escapeQuotes s =
      let (start, end) = fmap (BS.drop 1) $ BS.break (=='\'') s
      in start <> "''" <> escapeQuotes end

instance ToRow backend One String where
  toRow _ s = [formatString $ BS.pack s]

instance ToRow backend One BS.ByteString where
  toRow _ s = [formatString s]

instance ToRow backend One BSL.ByteString where
  toRow _ s = [formatString $ BSL.toStrict s]

instance ToRow backend One T.Text where
  toRow _ s = [formatString $ TE.encodeUtf8 s]

instance ToRow backend One TL.Text where
  toRow _ s = [formatString $ TE.encodeUtf8 $ TL.toStrict s]

instance ToRow backend One Int where
  toRow _ n = [BS.pack $ show n]

instance ToRow backend One Int8 where
  toRow _ n = [BS.pack $ show n]

instance ToRow backend One Int16 where
  toRow _ n = [BS.pack $ show n]

instance ToRow backend One Int32 where
  toRow _ n = [BS.pack $ show n]

instance ToRow backend One Int64 where
  toRow _ n = [BS.pack $ show n]

instance ToRow backend One Integer where
  toRow _ n = [BS.pack $ show n]

instance ToRow backend One Double where
  toRow _ n = [BS.pack $ show n]

instance ToRow backend One Float where
  toRow _ n = [BS.pack $ show n]

instance (NTimes (Vector n), Backend backend, ToRow backend n a)
  => ToRow backend n (Maybe a) where
  toRow backend = \case
    Nothing -> ntimes "NULL"
    Just x  -> toRow backend x

instance ( NTimes (Vector k), NTimes (Vector l), Backend backend
         , ToRow backend k a, ToRow backend l b, (k :+ l) ~ i )
  => ToRow backend i (a, b)

instance ( NTimes (Vector k), NTimes (Vector l), NTimes (Vector i)
         , Backend backend, ToRow backend k a, ToRow backend l b
         , ToRow backend i c, (k :+ (l :+ i)) ~ j )
  => ToRow backend j (a, b, c)
