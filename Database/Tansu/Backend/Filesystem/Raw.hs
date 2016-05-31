{-# LANGUAGE ViewPatterns #-}

module Database.Tansu.Backend.Filesystem.Raw (withRawFilesystemDb) where

import           Control.Exception
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Database.Tansu.Internal
import           System.Directory ( createDirectoryIfMissing
                                  , doesFileExist
                                  , removeFile
                                  )
import           System.FileLock ( SharedExclusive(Exclusive)
                                 , withFileLock
                                 )
import           System.FilePath ((</>), isValid)

catchIO :: IO a -> IO (Either TansuError a)
catchIO mote = fmap return mote `catch` go
  where go :: IOException -> IO (Either TansuError a)
        go = return . Left . OtherError . show

filePathSet :: FilePath -> ByteString -> ByteString -> IO (Either TansuError ())
filePathSet path bsKey val = do
  let key = BS.unpack bsKey
      keyPath = path </> key
  if not (isValid key)
     then return (Left (OtherError ("Invalid file name: " ++ key)))
     else do
       let keyPath = path </> key
       catchIO $ BS.writeFile keyPath val

filePathGet :: FilePath -> ByteString -> IO (Either TansuError ByteString)
filePathGet path bsKey = do
  let key = BS.unpack bsKey
      keyPath = path </> key
  if not (isValid key)
     then return (Left (OtherError ("Invalid file name: " ++ key)))
     else do
       exists <- doesFileExist keyPath
       if exists
         then Right `fmap` BS.readFile keyPath
         else return (Left (KeyNotFound bsKey))

filePathDel :: FilePath -> ByteString -> IO (Either TansuError ())
filePathDel path bsKey = do
  let key = BS.unpack bsKey
      keyPath = path </> key
  if not (isValid key)
     then return (Left (OtherError ("Invalid file name: " ++ key)))
     else catchIO $ removeFile keyPath

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
withRawFilesystemDb :: FilePath -> (TansuDb -> IO a) -> IO a
withRawFilesystemDb path comp = do
  createDirectoryIfMissing True path
  comp $ TansuDb { dbSet            = filePathSet path
                 , dbGet            = filePathGet path
                 , dbDel            = filePathDel path
                 , dbRunTransaction = filePathLock path
                 }
