{-# LANGUAGE Rank2Types #-}

module Database.Tansu.Internal ( TansuDb(..)
                               , TansuError(..)
                               ) where

import Data.ByteString (ByteString)

-- | The 'TansuError' type enumerates the possible errors that
--   can arise in the process of running a sequence of database
--   accesses. Backends should attempt to use the most informative
--   error available, but the 'OtherError' constructor is provided
--   as a catch-all if none of the other constructors are
--   descriptive of the error in question.
data TansuError
  = KeyNotFound ByteString
  | DecodeError String
  | AccessError String
  | OtherError String
    deriving (Eq, Show)

-- | The data representation of a Tansu backend. Users of the library
--   should treat this as an abstract type, but the full definition
--   is exposed by the "Database.Tansu.Internal" module so that
--   other libraries can implement new storage backends.
data TansuDb = TansuDb
  { dbSet            :: ByteString -> ByteString -> IO (Either TansuError ())
  , dbGet            :: ByteString -> IO (Either TansuError ByteString)
  , dbDel            :: ByteString -> IO (Either TansuError ())
  , dbRunTransaction :: forall a. IO a -> IO a
  }
