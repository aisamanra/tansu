# Tansu

**WARNING: DO NOT USE THIS CODE.** It's seriously like almost totally
untested, very much not finished, and I reserve the right to modify it
in part or in total at any time.

The Tansu library is a minimal API for storing and recalling data in
key-value storage backends. The Tansu library does not intend to be useful
for working with pre-existing data, as it makes assumptions about the
formatting of keys and values.

## Example

~~~.haskell
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)

import Database.Tansu ((=:), get, run)
import Database.Tansu.Backend.Filesystem (withFilesystemDb)

data Person = Person { name :: String, age  :: Int }
  deriving (Eq, Show, Generic, Serialize)

main :: IO ()
main = withFilesystemDb "sample.db" $ \ db -> do
  run db $ do
    "alex"  =: Person "alex" 33
    "blake" =: Person "blake" 22

  Right age <- run db (age `fmap` get "blake")
  putStrLn $ "Blake's age is " ++ show age
~~~

## Use

The Tansu API is very small and simple. All keys and values must implement
the `Serialize` typeclass from the
[`cereal`](https://hackage.haskell.org/package/cereal)
library. No type information is saved in the key-value store, so care must
be taken to ensure that the correct deserializer is being used when a value
is extracted from the backing store.

A value of type `TansuDb` represents a given key-value mapping. The only
way to interact with a `TansuDb` is by running a `Tansu` command, which
represents a (possibly empty) sequence of stores and loads applied
successively to the key-value mapping. Values can be set using the `set`
command (or its operator synonym `=:`), retrieved using the `get`
command, and deleted using the `del` command.

~~~.haskell
-- set a key to a value
set   :: (Serialize k, Serialize v) => k -> v -> Tansu ()

-- infix alias for set
(=:)  :: (Serialize k, Serialize v) => k -> v -> Tansu ()

-- get a value, failing if it does not exist
get   :: (Serialize k, Serialize v) => k -> Tansu v

-- get a value, returning Nothing if it does not exist
getMb :: (Serialize k, Serialize v) => k -> Tansu (Maybe v)

-- remove a key and its associated value
del   :: (Serialize k) => k -> Tansu ()

-- run a Tansu computation
run   :: TansuDb -> Tansu a -> IO (Either TansuError a)
~~~

A value of type `TansuDb` should be supplied by a _backend_, which can
correspond to any storage medium capable of storing a key-value store.
The `tansu` library only defines two trivial storage backends—one that
offers a trivial in-memory key-value mapping without any persistence at
all, and another that naïvely stores the key-value mapping as files in a
local directory. However, `tansu` is designed in such a way that storage
backends can be simply implemented separately from the core `tansu`
library, and backends can be easily swapped out as desired.

## About the Name

A _tansu_ is a kind of
[mobile Japanese wooden cabinet](https://en.wikipedia.org/wiki/Tansu),
and the initial plan for the _tansu_ library was for it to be a convenient
API wrapper over the [Kyoto Cabinet](http://fallabs.com/kyotocabinet/)
library, but it has since become a generic wrapper over various
key-value mapping backends. It is still a kind of storage system.
