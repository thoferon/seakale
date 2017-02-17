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
    case (colInfoType, fieldValue) of
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
    case (BS.splitAt 1 colInfoType, fieldValue) of
      (("_", typ), Just bs) ->
        pbackend `pbind` \backend ->
        arrayParser backend (col { colInfoType = typ }) bs
      (_, Just _) -> pfail $ "invalid type for list: " ++ BS.unpack colInfoType
      (_, Nothing) -> pfail "unexpected NULL for list"

-- FIXME: What about \n for example?
arrayParser :: FromRow PSQL One a => PSQL -> ColumnInfo PSQL -> BS.ByteString
            -> RowParser PSQL Zero [a]
arrayParser backend col = either pfail preturn . go
  where
    go :: FromRow PSQL One a => BS.ByteString -> Either String [a]
    go bs = case BS.splitAt 1 bs of
      ("{", "}") -> return []
      ("{", bs') -> readValues id bs'
      _ -> Left $ "invalid array starting with " ++ show (BS.take 30 bs)

    readValues :: FromRow PSQL One a => ([a] -> [a]) -> BS.ByteString
               -> Either String [a]
    readValues f bs = do
      (valBS, bs') <- readByteString bs
      let mValBS = if valBS == "NULL" then Nothing else Just valBS
      val <- parseRow fromRow backend [col] [Field mValBS]
      case BS.splitAt 1 bs' of
        (",", bs'') -> readValues (f . (val :)) bs''
        ("}", "")   -> return $! f [val]
        _ -> Left $ "invalid array around " ++ show (BS.take 30 bs')

    readByteString :: BS.ByteString
                   -> Either String (BS.ByteString, BS.ByteString)
    readByteString bs = case BS.splitAt 1 bs of
      ("\"", bs') -> readByteString' "" bs'
      _ -> return $ BS.span (\c -> c /= ',' && c /= '}') bs

    readByteString' :: BS.ByteString -> BS.ByteString
                    -> Either String (BS.ByteString, BS.ByteString)
    readByteString' acc bs =
      case fmap (BS.splitAt 1) (BS.span (\c -> c /= '"' && c /= '\\') bs) of
        (bs', ("\"", bs'')) -> return (acc <> bs', bs'')
        (bs', ("\\", bs'')) -> let (c, bs''') = BS.splitAt 1 bs''
                               in readByteString' (acc <> bs' <> c) bs'''
        (bs', _) -> Left $ "unreadable value around " ++ show (BS.take 30 bs')
