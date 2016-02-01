module Database.Tansu.Backend.Ephemeral
         ( EphemeralDb
         , withEphemeralDb
         , withNewEphemeralDb
         , createEphemeralDb
         ) where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.IORef
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Serialize (Serialize, encode)
import Database.Tansu (Tansu)
import Database.Tansu.Internal (TansuDb(..))

type Table = M.Map ByteString ByteString

ephemeralRunTransaction :: MVar () -> IO a -> IO a
ephemeralRunTransaction lock comp = withMVar lock $ \ () -> comp

ephemeralSet :: IORef Table -> ByteString -> ByteString -> IO ()
ephemeralSet table key val = modifyIORef table (M.insert key val)

ephemeralGet :: IORef Table -> ByteString -> IO (Maybe ByteString)
ephemeralGet table key = M.lookup key `fmap` readIORef table

ephemeralDel :: IORef Table -> ByteString -> IO ()
ephemeralDel table key = modifyIORef table (M.delete key)

-- | An 'EphemeralDb' is just an in-memory map, with no way of saving it.
--   It is intended to be used for testing Tansu code.
newtype EphemeralDb = EDB { fromEDB :: Table }

-- | Create an in-memory table to use for testing.
createEphemeralDb :: (Serialize k, Serialize v) => [(k, v)] -> EphemeralDb
createEphemeralDb ls = EDB (M.fromList [ (encode k, encode v) | (k, v) <- ls ])

-- | Run a 'Tansu' operation with an empty in-memory table.
withNewEphemeralDb :: (TansuDb -> IO a) -> IO a
withNewEphemeralDb = withEphemeralDb $ EDB M.empty

-- | Run a 'Tansu' operation with an existing in-memory table.
withEphemeralDb :: EphemeralDb -> (TansuDb -> IO a) -> IO a
withEphemeralDb init comp = do
  lock  <- newMVar ()
  table <- newIORef (fromEDB init)
  comp $ TansuDb
    { dbRunTransaction = ephemeralRunTransaction lock
    , dbSet = ephemeralSet table
    , dbGet = ephemeralGet table
    , dbDel = ephemeralDel table
    }
