{-# LANGUAGE RankNTypes #-}

module Database.Tansu ( -- * The 'Tansu' monad
                        Tansu
                      , TansuDb
                      , TansuError(..)
                        -- * 'Tansu' Operations
                      , get
                      , getMb
                      , set
                      , (=:)
                      , del
                        -- * Running a 'Tansu' operation
                      , run
                      ) where

import Data.ByteString (ByteString)
import Data.Serialize (Serialize, encode, decode)
import MonadLib (raise, try)
import Database.Tansu.Monad
import Database.Tansu.Internal

newtype Tansu a = Tansu { runTansu :: TansuM a }

instance Functor Tansu where
  fmap f (Tansu t) = Tansu (fmap f t)

instance Applicative Tansu where
  pure = Tansu . pure
  Tansu f <*> Tansu x = Tansu (f <*> x)

instance Monad Tansu where
  Tansu x >>= f = Tansu (x >>= runTansu . f)

-- | Sets the value for a key to a value.
set :: ByteString -> ByteString -> Tansu ()
set key val = Tansu $ setInternal key val

-- | Sets the value for a key to a value. A convenience operator
--   that is identical to 'set'.
(=:) :: ByteString -> ByteString -> Tansu ()
(=:) = set

-- | Gets a value for a given key. The resulting 'Tansu' computation
--   will fail if the key is not present in the storage backend.
get :: ByteString -> Tansu ByteString
get key = Tansu $ getInternal key

-- | Gets a value for a given key. If the key is not present, this
--   computation will return 'Nothing' instead. Other errors, such
--   as problems decoding the serialized value or difficulties
--   communicating with the storage backend, will still cause the
--   'Tansu' computation to fail.
getMb :: ByteString -> Tansu (Maybe ByteString)
getMb key = do
  rs <- Tansu $ try $ runTansu $ get key
  case rs of
    Left (KeyNotFound _) -> return Nothing
    Left err             -> Tansu (raise err)
    Right val            -> return (Just val)

-- | Delete a key.
del :: ByteString -> Tansu ()
del key = Tansu $ delInternal key

-- | Given a storage backend and a 'Tansu' computation, execute the
--   sequence of 'get' and 'set' commands and produce either the value
--   or the error encountered while running the computation.
run :: TansuDb -> Tansu a -> IO (Either TansuError a)
run db (Tansu mote) = runInternal db mote
