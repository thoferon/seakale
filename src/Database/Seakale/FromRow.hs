module Database.Seakale.FromRow
  ( RowParser
  , pmap, ppure, preturn, papply, pbind, pfail, pempty, por
  , pbackend, pconsume
  , FromRow(..)
  , parseRows
  , parseRow
  , Null(..)
  , maybeParser
  ) where

import           GHC.Generics
import           GHC.Int

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Data.Text.Lazy        as TL

import           Database.Seakale.Types

-- Previous attempts to define it the "normal" way (and making it a Monad, ...)
-- failed because GHC doesn't know that the type-level sum is associative
-- resulting in errors such as:
--
-- Could not deduce ((k :+ (l :+ m)) ~ (i :+ m))
-- from the context (GFromRow backend k a,
--                   GFromRow backend l b,
--                   (k :+ l) ~ i)
data RowParser backend :: Nat -> * -> * where
  GetBackend :: RowParser backend Zero backend
  Consume    :: RowParser backend One (ColumnInfo backend, Field backend)
  Pure       :: a -> RowParser backend Zero a
  Bind       :: RowParser backend n a -> (a -> RowParser backend m b)
             -> RowParser backend (n :+ m) b
  Or         :: RowParser backend n a -> RowParser backend n a
             -> RowParser backend n a
  Fail       :: String -> RowParser backend n a

pmap :: (a -> b) -> RowParser backend n a -> RowParser backend n b
pmap f = \case
  GetBackend    -> Bind GetBackend $ \x -> Pure $ f x
  Consume       -> Bind Consume $ \x -> Pure $ f x
  Pure x        -> Pure $ f x
  Bind parser g -> Bind parser $ \x -> pmap f $ g x
  Or par1 par2  -> Or (fmap f par1) (fmap f par2)
  Fail msg      -> Fail msg

instance Functor (RowParser backend n) where
  fmap = pmap

ppure, preturn :: a -> RowParser backend Zero a
ppure = Pure
preturn = ppure

papply :: RowParser backend n (a -> b) -> RowParser backend m a
       -> RowParser backend (n :+ m) b
papply pf px = Bind pf $ \f -> pmap (\x -> f x) px

pbind :: RowParser backend n a -> (a -> RowParser backend m b)
      -> RowParser backend (n :+ m) b
pbind = Bind

pfail :: String -> RowParser backend n a
pfail = Fail

pempty :: RowParser backend n a
pempty = pfail "pempty"

por :: RowParser backend n a -> RowParser backend n a -> RowParser backend n a
por = Or

pbackend :: RowParser backend Zero backend
pbackend = GetBackend

pconsume :: RowParser backend One (ColumnInfo backend, Field backend)
pconsume = Consume

execParser :: RowParser backend n a -> backend
           -> [(ColumnInfo backend, Field backend)]
           -> Either String ([(ColumnInfo backend, Field backend)], a)
execParser parser backend pairs = case parser of
  Pure x  -> return (pairs, x)
  GetBackend -> return (pairs, backend)
  Consume -> case pairs of
    pair : pairs' -> return (pairs', pair)
    [] -> Left "not enough columns"
  Bind parser' f -> do
    (pairs', x) <- execParser parser' backend pairs
    execParser (f x) backend pairs'
  Or parser1 parser2 ->
    let eRes1 = execParser parser1 backend pairs
        eRes2 = execParser parser2 backend pairs
    in case eRes1 of
      Right _ -> eRes1
      _ -> eRes2
  Fail msg -> Left msg

class FromRow backend n a | a -> n where
  fromRow :: RowParser backend n a

  default fromRow :: (Generic a, GFromRow backend ReadCon n (Rep a))
                  => RowParser backend (n :+ Zero) a
  fromRow =
    gfromRow ReadCon Nothing `pbind` \case
      Nothing -> pfail "GFromRow backend ?: error while parsing"
      Just x  -> ppure (to x)

data ReadCon = ReadCon
newtype DontReadCon = DontReadCon BS.ByteString

class GFromRow backend con n f | f -> n where
  gfromRow :: con -> Maybe BS.ByteString -> RowParser backend n (Maybe (f a))

instance GFromRow backend con Zero V1 where
  gfromRow _ _ = pfail "GFromRow backend V1: no value for GHC.Generic.V1"

instance GFromRow backend con Zero U1 where
  gfromRow _ _ = ppure $ Just U1

instance (GFromRow backend con k a, GFromRow backend con l b, (k :+ l) ~ i)
  => GFromRow backend con i (a :*: b) where
  gfromRow dbCon brCon =
    gfromRow dbCon brCon `pbind` \ma ->
      flip pmap (gfromRow dbCon brCon) (\mb -> ((:*:) <$> ma <*> mb))

instance (GFromRow backend DontReadCon k (a :+: b), 'S k ~ i)
  => GFromRow backend ReadCon i (a :+: b) where
  gfromRow ReadCon brCon =
    pconsume `pbind` \(_, f) -> case fieldValue f of
      Nothing ->
        pfail "GFromRow backend (a :+: b): found NULL in place of constructor"
      Just con -> gfromRow (DontReadCon con) brCon

instance ( GFromRow backend DontReadCon k a, GFromRow backend DontReadCon l b
         , (k :+ l) ~ i )
  => GFromRow backend DontReadCon i (a :+: b) where
  gfromRow dbCon brCon =
    gfromRow dbCon brCon `pbind` \ml ->
      flip pmap (gfromRow dbCon brCon) $ \mr -> case (ml, mr) of
        (Just l, _) -> Just $ L1 l
        (_, Just r) -> Just $ R1 r
        _ -> Nothing

instance (FromRow backend n a, SkipColumns backend n)
  => GFromRow backend DontReadCon n (K1 i a) where
  gfromRow (DontReadCon con) = \case
    Just con' | con == con' -> pmap (Just. K1) fromRow
    _ -> pmap (const Nothing) skipColumns

instance FromRow backend n a => GFromRow backend ReadCon n (K1 i a) where
  gfromRow ReadCon _ = pmap (Just. K1) fromRow

instance (Constructor c, GFromRow backend ReadCon n a)
  => GFromRow backend ReadCon n (M1 C c a) where
  gfromRow dbCon _ = go undefined
    where
      go :: (Constructor c, GFromRow backend ReadCon n a)
         => M1 C c a b -> RowParser backend n (Maybe (M1 C c a b))
      go m1 = pmap (fmap M1) $ gfromRow dbCon $ Just $ BS.pack $ conName m1

instance (Constructor c, GFromRow backend DontReadCon n a)
  => GFromRow backend DontReadCon n (M1 C c a) where
  gfromRow dbCon _ = go undefined
    where
      go :: (Constructor c, GFromRow backend DontReadCon n a)
         => M1 C c a b -> RowParser backend n (Maybe (M1 C c a b))
      go m1 = pmap (fmap M1) $ gfromRow dbCon $ Just $ BS.pack $ conName m1

instance GFromRow backend con n a => GFromRow backend con n (M1 D c a) where
  gfromRow dbCon brCon = pmap (fmap M1) (gfromRow dbCon brCon)

instance GFromRow backend con n a => GFromRow backend con n (M1 S c a) where
  gfromRow dbCon brCon = pmap (fmap M1) (gfromRow dbCon brCon)

class SkipColumns backend n where
  skipColumns :: RowParser backend n ()

instance SkipColumns backend Zero where
  skipColumns = ppure ()

instance (SkipColumns backend n, 'S n ~ m) => SkipColumns backend m where
  skipColumns = pconsume `pbind` \_ -> skipColumns

parseRows :: RowParser backend n a -> backend -> [ColumnInfo backend]
          -> [Row backend] -> Either String [a]
parseRows parser backend cols = mapM (parseRow parser backend cols)

parseRow :: RowParser backend n a -> backend -> [ColumnInfo backend]
         -> Row backend -> Either String a
parseRow parser backend cols row = do
  let pairs = zip cols row
  snd <$> execParser parser backend pairs

data Null = Null

instance FromRow backend One Null where
  fromRow = pconsume `pbind` \(_, f) -> case fieldValue f of
    Nothing -> preturn Null
    Just _  -> pfail "expected NULL"

instance FromRow backend Zero (Vector Zero a) where
  fromRow = preturn Nil

instance (FromRow backend One a, FromRow backend n (Vector n a))
  => FromRow backend ('S n) (Vector ('S n) a) where
  fromRow = fromRow `pbind` \x -> pmap (\xs -> x `cons` xs) fromRow

instance Backend backend => FromRow backend Zero ()

bytestringParser :: RowParser backend One BS.ByteString
bytestringParser = pconsume `pbind` \(_, f) -> case fieldValue f of
  Nothing -> pfail "unexpected NULL"
  Just bs -> preturn bs

readerParser :: Read a => RowParser backend One a
readerParser = pconsume `pbind` \(_, f) -> case fieldValue f of
  Nothing -> pfail "unexpected NULL"
  Just bs ->
    let str = BS.unpack bs
    in case reads str of
      (x,""):_ -> preturn x
      _ -> pfail $ "unreadable value: " ++ str

instance FromRow backend One String where
  fromRow = pmap BS.unpack bytestringParser

instance FromRow backend One BS.ByteString where
  fromRow = bytestringParser

instance FromRow backend One BSL.ByteString where
  fromRow = pmap BSL.fromStrict bytestringParser

instance FromRow backend One T.Text where
  fromRow = pmap TE.decodeUtf8 bytestringParser

instance FromRow backend One TL.Text where
  fromRow = pmap (TL.fromStrict . TE.decodeUtf8) bytestringParser

instance FromRow backend One Int where
  fromRow = readerParser

instance FromRow backend One Int8 where
  fromRow = readerParser

instance FromRow backend One Int16 where
  fromRow = readerParser

instance FromRow backend One Int32 where
  fromRow = readerParser

instance FromRow backend One Int64 where
  fromRow = readerParser

instance FromRow backend One Integer where
  fromRow = readerParser

instance FromRow backend One Double where
  fromRow = readerParser

instance FromRow backend One Float where
  fromRow = readerParser

maybeParser :: forall backend k a. FromRow backend k (Vector k Null)
            => RowParser backend k a -> RowParser backend k (Maybe a)
maybeParser parser =
  pmap Just parser
  `por`
  (pmap (\_ -> Nothing) (fromRow :: RowParser backend k (Vector k Null)))

instance (FromRow backend k a, FromRow backend k (Vector k Null))
  => FromRow backend k (Maybe a) where
  fromRow = maybeParser fromRow

instance (FromRow backend k a, FromRow backend l b, (k :+ l) ~ i)
  => FromRow backend i (a, b) where
  fromRow = (,) `pmap` fromRow `papply` fromRow

instance ( FromRow backend k a, FromRow backend l b, FromRow backend i c
         , (k :+ l :+ i) ~ j )
  => FromRow backend j (a, b, c) where
  fromRow = (,,) `pmap` fromRow `papply` fromRow `papply` fromRow
