module Database.Tansu.Backend.Ephemeral
         (withEphemeralDb) where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.IORef
import qualified Data.Map.Strict as M
import Database.Tansu.Internal (TansuDb(..))

type Table = M.Map ByteString ByteString

ephemeralRunTransaction :: MVar () -> IO a -> IO a
ephemeralRunTransaction lock comp = withMVar lock $ \ () -> comp

ephemeralSet :: IORef Table -> ByteString -> ByteString -> IO ()
ephemeralSet table key val = modifyIORef table (M.insert key val)

ephemeralGet :: IORef Table -> ByteString -> IO (Maybe ByteString)
ephemeralGet table key = M.lookup key `fmap` readIORef table

withEphemeralDb :: (TansuDb -> IO a) -> IO a
withEphemeralDb comp = do
  lock  <- newMVar ()
  table <- newIORef M.empty
  comp $ TansuDb
    { dbRunTransaction = ephemeralRunTransaction lock
    , dbSet = ephemeralSet table
    , dbGet = ephemeralGet table
    }
