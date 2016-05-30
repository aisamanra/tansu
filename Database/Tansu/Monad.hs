module Database.Tansu.Monad where

import Data.ByteString (ByteString)
import MonadLib ( ReaderT
                , ExceptionT
                , ask
                , raise
                , try
                , inBase
                , runExceptionT
                , runReaderT
                )
import Database.Tansu.Internal

type TansuM a = ReaderT TansuDb (ExceptionT TansuError IO) a

propagate :: IO (Either TansuError a) -> TansuM a
propagate mote = do
  rs <- inBase $ mote
  case rs of
    Left err -> raise err
    Right x  -> return x

setInternal :: ByteString -> ByteString -> TansuM ()
setInternal key val = do
  db <- ask
  propagate $ dbSet db key val

getInternal :: ByteString -> TansuM ByteString
getInternal key = do
  db <- ask
  propagate $ dbGet db key

delInternal :: ByteString -> TansuM ()
delInternal key = do
  db <- ask
  propagate $ dbDel db key

runInternal :: TansuDb -> TansuM a -> IO (Either TansuError a)
runInternal db mote =
  dbRunTransaction db (runExceptionT (runReaderT db mote))
