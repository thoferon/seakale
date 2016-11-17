module Database.Seakale.ToRow where

import qualified Data.ByteString as BS

import           Database.Seakale.Types

class ToField backend a where
  toField :: Proxy backend -> a -> BS.ByteString

class ToRow backend n a | a -> n where
  toRow :: Proxy backend -> a -> QueryData n

instance ToRow backend Zero () where
  toRow _ () = Nil

instance ToField backend a => ToRow backend One (Only a) where
  toRow proxy (Only k) = toField proxy k <:> Nil

instance (ToField backend a, ToField backend b)
  => ToRow backend Two (a, b) where
  toRow proxy (k, l) = toField proxy k <:| toField proxy l

instance (ToField backend a, ToField backend b, ToField backend c)
  => ToRow backend Three (a, b, c) where
  toRow proxy (k, l, m) =
    toField proxy k <:> toField proxy l <:| toField proxy m

instance ( ToField backend a, ToField backend b, ToField backend c
         , ToField backend d ) => ToRow backend Four (a, b, c, d) where
  toRow proxy (k, l, m, n) =
    toField proxy k <:> toField proxy l <:> toField proxy m <:| toField proxy n

instance ( ToField backend a, ToField backend b, ToField backend c
         , ToField backend d, ToField backend e )
  => ToRow backend Five (a, b, c, d, e) where
  toRow proxy (k, l, m, n, o) =
    toField proxy k <:> toField proxy l <:> toField proxy m <:> toField proxy n
    <:| toField proxy o

instance ( ToField backend a, ToField backend b, ToField backend c
         , ToField backend d, ToField backend e, ToField backend f )
  => ToRow backend Six (a, b, c, d, e, f) where
  toRow proxy (k, l, m, n, o, p) =
    toField proxy k <:> toField proxy l <:> toField proxy m <:> toField proxy n
    <:> toField proxy o <:| toField proxy p

instance ( ToField backend a, ToField backend b, ToField backend c
         , ToField backend d, ToField backend e, ToField backend f
         , ToField backend g )
  => ToRow backend Seven (a, b, c, d, e, f, g) where
  toRow proxy (k, l, m, n, o, p, q) =
    toField proxy k <:> toField proxy l <:> toField proxy m <:> toField proxy n
    <:> toField proxy o <:> toField proxy p <:| toField proxy q

instance ( ToField backend a, ToField backend b, ToField backend c
         , ToField backend d, ToField backend e, ToField backend f
         , ToField backend g, ToField backend h )
  => ToRow backend Eight (a, b, c, d, e, f, g, h) where
  toRow proxy (k, l, m, n, o, p, q, r) =
    toField proxy k <:> toField proxy l <:> toField proxy m <:> toField proxy n
    <:> toField proxy o <:> toField proxy p <:> toField proxy q
    <:| toField proxy r

instance ( ToField backend a, ToField backend b, ToField backend c
         , ToField backend d, ToField backend e, ToField backend f
         , ToField backend g, ToField backend h, ToField backend i )
  => ToRow backend Nine (a, b, c, d, e, f, g, h, i) where
  toRow proxy (k, l, m, n, o, p, q, r, s) =
    toField proxy k <:> toField proxy l <:> toField proxy m <:> toField proxy n
    <:> toField proxy o <:> toField proxy p <:> toField proxy q
    <:> toField proxy r <:| toField proxy s

instance ( ToField backend a, ToField backend b, ToField backend c
         , ToField backend d, ToField backend e, ToField backend f
         , ToField backend g, ToField backend h, ToField backend i
         , ToField backend j )
  => ToRow backend Ten (a, b, c, d, e, f, g, h, i, j) where
  toRow proxy (k, l, m, n, o, p, q, r, s, t) =
    toField proxy k <:> toField proxy l <:> toField proxy m <:> toField proxy n
    <:> toField proxy o <:> toField proxy p <:> toField proxy q
    <:> toField proxy r <:> toField proxy s <:| toField proxy t

instance ToField backend a => ToRow backend n (Vector n a) where
  toRow proxy xs = fmap (toField proxy) xs
