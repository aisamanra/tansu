module Database.Tansu ( Tansu
                      , TansuDb
                      , TansuError(..)
                      , get
                      , getMb
                      , set
                      , (=:)
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

type TansuM a = ReaderT TansuDb (ExceptionT TansuError IO) a

newtype Tansu a = Tansu { runTansu :: TansuM a }

instance Functor Tansu where
  fmap f (Tansu t) = Tansu (fmap f t)

instance Applicative Tansu where
  pure = Tansu . pure
  Tansu f <*> Tansu x = Tansu (f <*> x)

instance Monad Tansu where
  Tansu x >>= f = Tansu (x >>= runTansu . f)

set :: (Serialize k, Serialize v) => k -> v -> Tansu ()
set key val = do
  db <- Tansu ask
  Tansu $ inBase $ dbSet db (encode key) (encode val)

(=:) :: (Serialize k, Serialize v) => k -> v -> Tansu ()
(=:) = set

get :: (Serialize k, Serialize v) => k -> Tansu v
get key = do
  result <- getMb key
  case result of
    Just val -> return val
    Nothing  -> Tansu (raise (KeyNotFound (encode key)))

getMb :: (Serialize k, Serialize v) => k -> Tansu (Maybe v)
getMb key = do
  db <- Tansu ask
  result <- Tansu $ inBase $ dbGet db (encode key)
  case result of
    Nothing -> return Nothing
    Just bs -> case decode bs of
      Right val' -> return (Just val')
      Left err   -> Tansu (raise (DecodeError err))

run :: TansuDb -> Tansu a -> IO (Either TansuError a)
run db (Tansu comp) =
  dbRunTransaction db (runExceptionT (runReaderT db comp))
