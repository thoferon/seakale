{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Seakale.PostgreSQL.FromRow
  ( module Database.Seakale.FromRow
  ) where

import           Data.Monoid
import           Data.Time
import qualified Data.ByteString.Char8 as BS

import           Database.Seakale.FromRow
import           Database.Seakale.PostgreSQL

instance FromRow PSQL One Bool where
  fromRow = pconsume `pbind` \(_, f) -> case fieldValue f of
    Nothing  -> pfail "unexpected NULL"
    Just "t" -> preturn True
    Just "f" -> preturn False
    Just bs  -> pfail $ "unreadable boolean: " ++ BS.unpack bs

instance FromRow PSQL One UTCTime where
  fromRow = pconsume `pbind` \(ColumnInfo{..}, Field{..}) ->
    case (typeName colInfoType, fieldValue) of
      ("timestamp", Just bs) ->
        case parseTimeM True defaultTimeLocale "%F %T%Q" (BS.unpack bs) of
          Just t  -> preturn t
          Nothing -> pfail $ "invalid time: " ++ BS.unpack bs
      ("timestamptz", Just bs) ->
        case parseTimeM True defaultTimeLocale "%F %T%Q%z"
               (BS.unpack bs ++ "00") of
          Just t  -> preturn t
          Nothing -> pfail $ "invalid time: " ++ BS.unpack bs
      (bs, Just _) -> pfail $ "invalid type for time: " ++ BS.unpack bs
      (_, Nothing) -> pfail "unexpected NULL for time"

instance FromRow PSQL One String where
  fromRow = pmap BS.unpack fromRow

instance {-# OVERLAPPABLE #-} FromRow PSQL One a => FromRow PSQL One [a] where
  fromRow = pconsume `pbind` \(col@ColumnInfo{..}, Field{..}) ->
    case (typeType colInfoType, fieldValue) of
      (TTArray subtype, Just bs) ->
        pbackend `pbind` \backend ->
          let col'  = col { colInfoType = subtype }
              f mBS = parseRow fromRow backend [col'] [Field mBS]
          in either pfail preturn $ seqParser '{' '}' "array" f bs
      (_, Just _) ->
        pfail $ "invalid type for list: " ++ BS.unpack (typeName colInfoType)
      (_, Nothing) -> pfail "unexpected NULL for list"

-- FIXME: What about \n for example?
seqParser :: Char -> Char -> String -> (Maybe BS.ByteString -> Either String a)
          -> BS.ByteString -> Either String [a]
seqParser ldelim rdelim descr h fullBS = case BS.uncons fullBS of
    Just (ldelim', rdelim')
      | ldelim == ldelim' && BS.singleton rdelim == rdelim' -> return []
    Just (ldelim', bs') | ldelim == ldelim' -> readValues h id bs'
    _ -> Left $ "invalid " ++ descr ++ " starting with "
                ++ show (BS.take 30 fullBS)

  where
    readValues :: (Maybe BS.ByteString -> Either String a) -> ([a] -> [a])
               -> BS.ByteString -> Either String [a]
    readValues f g bs = do
      (valBS, bs') <- readByteString bs
      let mValBS = if valBS == "NULL" then Nothing else Just valBS
      val <- f mValBS
      case BS.uncons bs' of
        Just (',', bs'') -> readValues f (g . (val :)) bs''
        Just (rdelim', "") | rdelim == rdelim' -> return $! g [val]
        _ -> Left $ "invalid " ++ descr ++ " around " ++ show (BS.take 30 bs')

    readByteString :: BS.ByteString
                   -> Either String (BS.ByteString, BS.ByteString)
    readByteString bs = case BS.uncons bs of
      Just ('"', bs') -> readByteString' "" bs'
      _ -> return $ BS.span (\c -> c /= ',' && c /= rdelim) bs

    readByteString' :: BS.ByteString -> BS.ByteString
                    -> Either String (BS.ByteString, BS.ByteString)
    readByteString' acc bs =
      case fmap BS.uncons (BS.span (\c -> c /= '"' && c /= '\\') bs) of
        (bs', Just ('"', bs'')) -> case BS.uncons bs'' of
          Just ('"', bs''') -> readByteString' (bs' <> "\"") bs'''
          _ -> return (acc <> bs', bs'')
        (bs', Just ('\\', bs'')) -> let (c, bs''') = BS.splitAt 1 bs''
                                    in readByteString' (acc <> bs' <> c) bs'''
        (bs', _) -> Left $ "unreadable value around " ++ show (BS.take 30 bs')

instance (Show a, FromRow PSQL n a) => FromRow PSQL One (Composite a) where
  fromRow = pconsume `pbind` \(ColumnInfo{..}, Field{..}) ->
    case (typeType colInfoType, fieldValue) of
      (TTComposite attrs, Just bs) -> pbackend `pbind` \backend ->
        either pfail preturn $ do
          let cols = map (\(name, tinfo) -> ColumnInfo (Just name) tinfo) attrs
          fields <- seqParser '(' ')' (BS.unpack (typeName colInfoType))
                              (Right . Field) bs
          Composite <$> parseRow fromRow backend cols fields
      (_, Just _) -> pfail $ "expected composite type"
      (_, Nothing) -> pfail "unexpected NULL for composite type"
