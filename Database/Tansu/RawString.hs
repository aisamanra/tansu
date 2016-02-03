{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Tansu.RawString ( RawString(..) ) where

import Control.DeepSeq ( NFData )
import Data.ByteString ( ByteString )
import Data.Serialize ( Serialize(..)
                      , getByteString
                      , putByteString
                      , remaining
                      )
import GHC.Exts ( IsString )

-- | A wrapper over 'ByteString' with a serialize instance
--   that just passes the bytestring on unchanged. It will
--   always be the case that
--
--   > encode bs == bs
--
--   and that
--
--   > decode bs == Right bs
newtype RawString =
  RawString { toByteString :: ByteString }
    deriving (Eq, Show, Ord, Read, IsString, Monoid, NFData)

instance Serialize RawString where
  put = putByteString . toByteString
  get = RawString `fmap` (getByteString =<< remaining)
