module Database.Tansu.Backend.Filesystem (withFilesystemDb) where

import Data.ByteString (ByteString)
import Data.ByteString.Base64
import qualified Data.ByteString.Char8 as BS
import Database.Tansu.Internal (Database(..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>))

filePathSet :: FilePath -> ByteString -> ByteString -> IO ()
filePathSet path key val = do
  let keyPath = path </> BS.unpack (encode key)
  BS.writeFile keyPath val

filePathGet :: FilePath -> ByteString -> IO (Maybe ByteString)
filePathGet path key = do
  let keyPath = path </> BS.unpack (encode key)
  fmap Just $ BS.readFile keyPath

withFilesystemDb :: FilePath -> (Database -> IO a) -> IO a
withFilesystemDb path comp = do
  createDirectoryIfMissing True path
  comp $ Database { dbSet = filePathSet path
                  , dbGet = filePathGet path
                  , dbRunTransaction = id
                  }
