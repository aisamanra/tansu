{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Database.Tansu
import Database.Tansu.Backend.Ephemeral
import Database.Tansu.Backend.Filesystem
import Database.Tansu.Backend.Filesystem.Raw

main :: IO ()
main = do
  putStrLn "Testing filesystem db"
  withFilesystemDb "sample-fs.db" sample

  putStrLn "Testing raw filesystem db"
  withRawFilesystemDb "sample-raw.db" sample

  putStrLn "Testing ephemeral db"
  withNewEphemeralDb sample

sample :: TansuDb -> IO ()
sample db = do
  putStrLn "Populating test database"
  run db $ do
    "one"   =: "un"
    "two"   =: "du"
    "three" =: "tri"
    "four"  =: "kvar"

  putStr "looking up key 'three': "
  rs <- run db $ get "three"
  case rs of
    Right val -> print val
    Left _    -> putStrLn "...not in the database."

  putStr "looking up key 'five': "
  rs <- run db $ get "five"
  case rs of
    Right val -> print val
    Left _    -> putStrLn "...not in the database."
