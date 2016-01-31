{-# LANGUAGE Rank2Types #-}

module Database.Tansu.Internal ( Database(..)
                               , TansuError(..)
                               ) where

import Data.ByteString (ByteString)

data TansuError
  = KeyNotFound ByteString
  | DecodeError String
    deriving (Eq, Show)

data Database = Database
  { dbSet            :: ByteString -> ByteString -> IO ()
  , dbGet            :: ByteString -> IO (Maybe ByteString)
  , dbRunTransaction :: forall a. IO a -> IO a
  }
