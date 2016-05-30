{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)

import Database.Tansu.Serialize ((=:), get, runS, serialized)
import Database.Tansu.Backend.Filesystem (withFilesystemDb)

data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Eq, Show, Generic, Serialize)

main :: IO ()
main = withFilesystemDb "sample.db" $ serialized $ \ db -> do
  runS db $ do
    "alex"  =: Person "alex" 33
    "blake" =: Person "blake" 22

  Right age <- runS db (age `fmap` get "blake")
  putStrLn $ "Blake's age is " ++ show age
