{-# LANGUAGE RankNTypes #-}

module Database.Tansu.Serialize
         ( -- * The 'TansuS' monad
           TansuS
         , TansuDbS
         , TansuError(..)
           -- * 'TansuS' Operations
         , get
         , getMb
         , set
         , (=:)
         , del
           -- * Running a 'TansuS' operation
         , runS
         , serialized
         ) where

import Data.ByteString (ByteString)
import Data.Serialize (Serialize, encode, decode)
import MonadLib (raise, try)

import Database.Tansu.Monad
import Database.Tansu.Internal

newtype TansuDbS k v = TansuDbS TansuDb

serialized :: (TansuDbS k v -> r) -> (TansuDb -> r)
serialized func = func . TansuDbS

newtype TansuS k v a = TansuS { runTansuS :: TansuM a }

instance Functor (TansuS k v) where
  fmap f (TansuS t) = TansuS (fmap f t)

instance Applicative (TansuS k v) where
  pure = TansuS . pure
  TansuS f <*> TansuS x = TansuS (f <*> x)

instance Monad (TansuS k v) where
  TansuS x >>= f = TansuS (x >>= runTansuS . f)

-- | Sets the value for a key to a value.
set :: (Serialize k, Serialize v) => k -> v -> TansuS k v ()
set key val = TansuS $ setInternal (encode key) (encode val)

-- | Sets the value for a key to a value. A convenience operator
--   that is identical to 'set'.
(=:) :: (Serialize k, Serialize v) => k -> v -> TansuS k v ()
(=:) = set

-- | Gets a value for a given key. The resulting 'Tansu' computation
--   will fail if the key is not present in the storage backend.
get :: (Serialize k, Serialize v) => k -> TansuS k v v
get key = do
  bs <- TansuS $ getInternal (encode key)
  case (decode bs) of
    Left err  -> TansuS (raise (DecodeError err))
    Right val -> return val

-- | Gets a value for a given key. If the key is not present, this
--   computation will return 'Nothing' instead. Other errors, such
--   as problems decoding the serialized value or difficulties
--   communicating with the storage backend, will still cause the
--   'Tansu' computation to fail.
getMb :: (Serialize k, Serialize v) => k -> TansuS k v (Maybe v)
getMb key = do
  rs <- TansuS $ try $ runTansuS $ get key
  case rs of
    Left (KeyNotFound _) -> return Nothing
    Left err             -> TansuS (raise err)
    Right val            -> return (Just val)

del :: (Serialize k) => k -> TansuS k v ()
del key = TansuS $ delInternal (encode key)

-- | Given a storage backend and a 'Tansu' computation, execute the
--   sequence of 'get' and 'set' commands and produce either the value
--   or the error encountered while running the computation.
runS :: TansuDbS k v -> TansuS k v a -> IO (Either TansuError a)
runS (TansuDbS db) (TansuS mote) = runInternal db mote
