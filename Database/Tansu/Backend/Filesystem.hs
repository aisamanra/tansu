module Database.Tansu.Backend.Filesystem (withFilesystemDb) where

import Data.ByteString (ByteString)
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as BS
import Database.Tansu.Internal (TansuDb(..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FileLock (SharedExclusive(Exclusive), withFileLock)
import System.FilePath.Posix ((</>))

filePathSet :: FilePath -> ByteString -> ByteString -> IO ()
filePathSet path key val = do
  let keyPath = path </> BS.unpack (encode key)
  BS.writeFile keyPath val

filePathGet :: FilePath -> ByteString -> IO (Maybe ByteString)
filePathGet path key = do
  let keyPath = path </> BS.unpack (encode key)
  exists <- doesFileExist keyPath
  if exists
    then Just `fmap` BS.readFile keyPath
    else return Nothing

filePathLock :: FilePath -> IO a -> IO a
filePathLock path comp = do
  withFileLock (path </> ".lock") Exclusive (const comp)

withFilesystemDb :: FilePath -> (TansuDb -> IO a) -> IO a
withFilesystemDb path comp = do
  createDirectoryIfMissing True path
  comp $ TansuDb { dbSet            = filePathSet path
                 , dbGet            = filePathGet path
                 , dbRunTransaction = filePathLock path
                 }
