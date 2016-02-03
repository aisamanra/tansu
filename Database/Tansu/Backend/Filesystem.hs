module Database.Tansu.Backend.Filesystem (withFilesystemDb) where

import Control.Exception
import Data.ByteString (ByteString)
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as BS
import Database.Tansu.Internal
import System.Directory ( createDirectoryIfMissing
                        , doesFileExist
                        , removeFile
                        )
import System.FileLock (SharedExclusive(Exclusive), withFileLock)
import System.FilePath.Posix ((</>))

catchIO :: IO a -> IO (Either TansuError a)
catchIO mote = fmap return mote `catch` go
  where go :: IOException -> IO (Either TansuError a)
        go = return . Left . OtherError . show

filePathSet :: FilePath -> ByteString -> ByteString -> IO (Either TansuError ())
filePathSet path key val = do
  let keyPath = path </> BS.unpack (encode key)
  catchIO $ BS.writeFile keyPath val

filePathGet :: FilePath -> ByteString -> IO (Either TansuError ByteString)
filePathGet path key = do
  let keyPath = path </> BS.unpack (encode key)
  exists <- doesFileExist keyPath
  if exists
    then Right `fmap` BS.readFile keyPath
    else return (Left (KeyNotFound key))

filePathDel :: FilePath -> ByteString -> IO (Either TansuError ())
filePathDel path key = do
  let keyPath = path </> BS.unpack (encode key)
  catchIO $ removeFile keyPath

filePathLock :: FilePath -> IO a -> IO a
filePathLock path comp = do
  withFileLock (path </> ".lock") Exclusive (const comp)

-- | Use a local directory as a key-value store. Each key-value
--   pair is represented as a file whose filename is the
--   Base64-encoded serialized key, and whose content is the
--   raw serialized value. Locking is done using the
--   @.lock@ file in the specified directory, but note that
--   file locking is not a guaranteed way of ensuring exclusion,
--   and that the files themselves are not locked in any way.
withFilesystemDb :: FilePath -> (TansuDb k v -> IO a) -> IO a
withFilesystemDb path comp = do
  createDirectoryIfMissing True path
  comp $ TansuDb { dbSet            = filePathSet path
                 , dbGet            = filePathGet path
                 , dbDel            = filePathDel path
                 , dbRunTransaction = filePathLock path
                 }
