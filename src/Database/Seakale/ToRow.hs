module Database.Seakale.ToRow where

import qualified Data.ByteString as BS

import           Database.Seakale.Types

class ToRow backend n a | a -> n where
  toRow :: backend -> a -> QueryData n

class ToField backend a where
  toField :: backend -> a -> BS.ByteString

instance ToRow backend Zero () where
  toRow _ () = Nil

instance ToField backend a => ToRow backend One (Only a) where
  toRow backend (Only k) = toField backend k <:> Nil

instance (ToField backend a, ToField backend b)
  => ToRow backend Two (a, b) where
  toRow backend (k, l) = toField backend k <:| toField backend l

instance (ToField backend a, ToField backend b, ToField backend c)
  => ToRow backend Three (a, b, c) where
  toRow backend (k, l, m) =
    toField backend k <:> toField backend l <:| toField backend m

instance ( ToField backend a, ToField backend b, ToField backend c
         , ToField backend d ) => ToRow backend Four (a, b, c, d) where
  toRow backend (k, l, m, n) =
    toField backend k <:> toField backend l <:> toField backend m
    <:| toField backend n

instance ( ToField backend a, ToField backend b, ToField backend c
         , ToField backend d, ToField backend e )
  => ToRow backend Five (a, b, c, d, e) where
  toRow backend (k, l, m, n, o) =
    toField backend k <:> toField backend l <:> toField backend m
    <:> toField backend n <:| toField backend o

instance ( ToField backend a, ToField backend b, ToField backend c
         , ToField backend d, ToField backend e, ToField backend f )
  => ToRow backend Six (a, b, c, d, e, f) where
  toRow backend (k, l, m, n, o, p) =
    toField backend k <:> toField backend l <:> toField backend m
    <:> toField backend n <:> toField backend o <:| toField backend p

instance ( ToField backend a, ToField backend b, ToField backend c
         , ToField backend d, ToField backend e, ToField backend f
         , ToField backend g )
  => ToRow backend Seven (a, b, c, d, e, f, g) where
  toRow backend (k, l, m, n, o, p, q) =
    toField backend k <:> toField backend l <:> toField backend m
    <:> toField backend n <:> toField backend o <:> toField backend p
    <:| toField backend q

instance ( ToField backend a, ToField backend b, ToField backend c
         , ToField backend d, ToField backend e, ToField backend f
         , ToField backend g, ToField backend h )
  => ToRow backend Eight (a, b, c, d, e, f, g, h) where
  toRow backend (k, l, m, n, o, p, q, r) =
    toField backend k <:> toField backend l <:> toField backend m
    <:> toField backend n <:> toField backend o <:> toField backend p
    <:> toField backend q <:| toField backend r

instance ( ToField backend a, ToField backend b, ToField backend c
         , ToField backend d, ToField backend e, ToField backend f
         , ToField backend g, ToField backend h, ToField backend i )
  => ToRow backend Nine (a, b, c, d, e, f, g, h, i) where
  toRow backend (k, l, m, n, o, p, q, r, s) =
    toField backend k <:> toField backend l <:> toField backend m
    <:> toField backend n <:> toField backend o <:> toField backend p
    <:> toField backend q <:> toField backend r <:| toField backend s

instance ( ToField backend a, ToField backend b, ToField backend c
         , ToField backend d, ToField backend e, ToField backend f
         , ToField backend g, ToField backend h, ToField backend i
         , ToField backend j )
  => ToRow backend Ten (a, b, c, d, e, f, g, h, i, j) where
  toRow backend (k, l, m, n, o, p, q, r, s, t) =
    toField backend k <:> toField backend l <:> toField backend m
    <:> toField backend n <:> toField backend o <:> toField backend p
    <:> toField backend q <:> toField backend r <:> toField backend s
    <:| toField backend t

instance ToField backend a => ToRow backend n (Vector n a) where
  toRow backend xs = fmap (toField backend) xs
