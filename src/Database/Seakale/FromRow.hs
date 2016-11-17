module Database.Seakale.FromRow where

import Control.Applicative
import Control.Monad

import Database.Seakale.Types

class FromRow backend a where
  fromRow :: RowParser backend a

class FromField backend a where
  fromField :: FieldParser backend a

type FieldParser backend a
  = ColumnInfo backend -> Field backend -> Either String a

data RowParser backend a
  = RowParser ([ColumnInfo backend] -> Row backend
                                    -> Either String ( [ColumnInfo backend]
                                                     , Row backend, a ))
  deriving Functor

instance Applicative (RowParser backend) where
  pure x = RowParser $ \cols rows -> Right (cols, rows, x)
  RowParser f <*> RowParser g = RowParser $ \cols rows -> do
    (cols',  rows',  h) <- f cols rows
    (cols'', rows'', x) <- g cols' rows'
    return (cols'', rows'', h x)

instance Monad (RowParser backend) where
  RowParser f >>= g = RowParser $ \cols rows -> do
    (cols', rows', x) <- f cols rows
    let RowParser h = g x
    h cols' rows'

instance Alternative (RowParser backend) where
  empty = RowParser $ \_ _ -> Left "empty"
  RowParser f <|> RowParser g = RowParser $ \cols rows ->
    case f cols rows of
      Left _ -> g cols rows
      res@(Right _) -> res

parseRow :: RowParser backend a -> [ColumnInfo backend] -> Row backend
         -> Either String a
parseRow (RowParser f) cols row = fmap (\(_, _, x) -> x) $ f cols row

parseRows :: RowParser backend a -> [ColumnInfo backend] -> [Row backend]
          -> Either String [a]
parseRows parser cols rows = mapM (parseRow parser cols) rows

field :: FromField backend a => RowParser backend a
field = fieldWith fromField

fieldWith :: FieldParser backend a -> RowParser backend a
fieldWith parser =
  RowParser $ curry $ \case
    (col : cols, f : row) -> fmap (cols,row,) (parser col f)
    _ -> Left "not enough columns"

numFieldsRemaining :: RowParser backend Int
numFieldsRemaining = RowParser $ \cols rows -> Right (cols, rows, length rows)

instance FromField backend () where
  fromField _ _ = return ()

instance FromField backend a => FromField backend (Maybe a) where
  fromField _ (Field { fieldValue = Nothing }) = return Nothing
  fromField typ f = Just <$> fromField typ f

instance FromRow backend () where
  fromRow = return ()

instance FromField backend a => FromRow backend (Only a) where
  fromRow = Only <$> field

instance (FromField backend a, FromField backend b)
  => FromRow backend (a, b) where
  fromRow = (,) <$> field <*> field

instance (FromField backend a, FromField backend b, FromField backend c)
  => FromRow backend (a, b, c) where
  fromRow = (,,) <$> field <*> field <*> field

instance ( FromField backend a, FromField backend b, FromField backend c
         , FromField backend d ) => FromRow backend (a, b, c, d) where
  fromRow = (,,,) <$> field <*> field <*> field <*> field

instance ( FromField backend a, FromField backend b, FromField backend c
         , FromField backend d, FromField backend e )
  => FromRow backend (a, b, c, d, e) where
  fromRow = (,,,,) <$> field <*> field <*> field <*> field <*> field

instance ( FromField backend a, FromField backend b, FromField backend c
         , FromField backend d, FromField backend e, FromField backend f )
  => FromRow backend (a, b, c, d, e, f) where
  fromRow = (,,,,,) <$> field <*> field <*> field <*> field <*> field <*> field

instance ( FromField backend a, FromField backend b, FromField backend c
         , FromField backend d, FromField backend e, FromField backend f
         , FromField backend g )
  => FromRow backend (a, b, c, d, e, f, g) where
  fromRow = (,,,,,,)
    <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ( FromField backend a, FromField backend b, FromField backend c
         , FromField backend d, FromField backend e, FromField backend f
         , FromField backend g, FromField backend h )
  => FromRow backend (a, b, c, d, e, f, g, h) where
  fromRow = (,,,,,,,)
    <$> field <*> field <*> field <*> field <*> field <*> field <*> field
    <*> field

instance ( FromField backend a, FromField backend b, FromField backend c
         , FromField backend d, FromField backend e, FromField backend f
         , FromField backend g, FromField backend h, FromField backend i )
  => FromRow backend (a, b, c, d, e, f, g, h, i) where
  fromRow = (,,,,,,,,)
    <$> field <*> field <*> field <*> field <*> field <*> field <*> field
    <*> field <*> field

instance ( FromField backend a, FromField backend b, FromField backend c
         , FromField backend d, FromField backend e, FromField backend f
         , FromField backend g, FromField backend h, FromField backend i
         , FromField backend j )
  => FromRow backend (a, b, c, d, e, f, g, h, i, j) where
  fromRow = (,,,,,,,,,)
    <$> field <*> field <*> field <*> field <*> field <*> field <*> field
    <*> field <*> field <*> field

instance FromField backend a => FromRow backend [a] where
  fromRow = do
    n <- numFieldsRemaining
    replicateM n field
