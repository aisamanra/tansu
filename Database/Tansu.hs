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
import MonadLib ( ReaderT
                , ExceptionT
                , ask
                , raise
                , inBase
                , runExceptionT
                , runReaderT
                )
import Database.Tansu.Internal

type TansuM k v a = ReaderT (TansuDb k v) (ExceptionT TansuError IO) a

-- | The 'Tansu' type is a monad which represents some sequence
--   of 'get' and 'set' operations. Ideally, backends should
--   make some guarantee about the atomicity of a given
--   'Tansu' computation, but you should consult the documentation
--   about a given backend to make sure that holds.
newtype Tansu k v a = Tansu { runTansu :: TansuM k v a }

instance Functor (Tansu k v) where
  fmap f (Tansu t) = Tansu (fmap f t)

instance Applicative (Tansu k v) where
  pure = Tansu . pure
  Tansu f <*> Tansu x = Tansu (f <*> x)

instance Monad (Tansu k v) where
  Tansu x >>= f = Tansu (x >>= runTansu . f)

propagate :: IO (Either TansuError a) -> Tansu k v a
propagate mote = do
  rs <- Tansu $ inBase $ mote
  case rs of
    Left err -> Tansu $ raise err
    Right x  -> return x

-- | Sets the value for a key to a value.
set :: (Serialize k, Serialize v) => k -> v -> Tansu k v ()
set key val = do
  db <- Tansu ask
  propagate $ dbSet db (encode key) (encode val)

-- | Sets the value for a key to a value. A convenience operator
--   that is identical to 'set'.
(=:) :: (Serialize k, Serialize v) => k -> v -> Tansu k v ()
(=:) = set

-- | Gets a value for a given key. The resulting 'Tansu' computation
--   will fail if the key is not present in the storage backend.
get :: (Serialize k, Serialize v) => k -> Tansu k v v
get key = do
  result <- getMb key
  case result of
    Just val -> return val
    Nothing  -> Tansu (raise (KeyNotFound (encode key)))

-- | Gets a value for a given key. If the key is not present, this
--   computation will return 'Nothing' instead. Other errors, such
--   as problems decoding the serialized value or difficulties
--   communicating with the storage backend, will still cause the
--   'Tansu' computation to fail.
getMb :: (Serialize k, Serialize v) => k -> Tansu k v (Maybe v)
getMb key = do
  db <- Tansu ask
  result <- Tansu $ inBase $ dbGet db (encode key)
  case result of
    Left (KeyNotFound _) -> return Nothing
    Left err             -> Tansu (raise err)
    Right bs -> case decode bs of
      Right val' -> return (Just val')
      Left err   -> Tansu (raise (DecodeError err))

del :: (Serialize k) => k -> Tansu k v ()
del key = do
  db <- Tansu ask
  propagate $ dbDel db (encode key)

-- | Given a storage backend and a 'Tansu' computation, execute the
--   sequence of 'get' and 'set' commands and produce either the value
--   or the error encountered while running the computation.
run :: TansuDb k v -> Tansu k v a -> IO (Either TansuError a)
run db (Tansu comp) =
  dbRunTransaction db (runExceptionT (runReaderT db comp))
