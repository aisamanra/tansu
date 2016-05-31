# Tansu

**WARNING: DO NOT USE THIS CODE.** It's seriously like almost totally
untested, very much not finished, and I reserve the right to modify it
in part or in total at any time.

The Tansu library is a minimal API for storing and recalling data in
key-value storage backends. It is designed for new applications that
have flexibility in terms of how they store information, as it may
not provide the appropriate tools for working with data in existing
key-value store backends.

## Example

~~~.haskell
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Main where

import Data.Serialize (Serialize)
import GHC.Generics (Generic)

import Database.Tansu.Serialize ((=:), get, runS, serialized)
import Database.Tansu.Backend.Filesystem (withFilesystemDb)

data Person = Person { name :: String, age  :: Int }
  deriving (Eq, Show, Generic, Serialize)

main :: IO ()
main = withFilesystemDb "sample.db" $ serialized $ \ db -> do
  runS db $ do
    "alex"  =: Person "Alex" 33
    "blake" =: Person "Blake" 22

  Right age <- run db (age `fmap` get "blake")
  putStrLn $ "Blake's age is " ++ show age
~~~

## Use

The `tansu` library exposes two near-identical APIs: the API
in `Database.Tansu` allows you to store and write strict `ByteString`
values, while the API in `Database.Tansu.Serialize` uses the typeclasses
associated with the `cereal` library to allow a wider range of types to
function as keys and values. The two libraries are otherwise very
similar.

A value of type `TansuDb` represents a given key-value mapping. The only
way to interact with a `TansuDb` is by running a `Tansu` command, which
represents a (possibly empty) sequence of stores and loads applied
successively to the key-value mapping. Values can be set using the `set`
command (or its operator synonym `=:`), retrieved using the `get`
command, and deleted using the `del` command.

~~~.haskell
-- set a key to a value
set   :: ByteString -> ByteString -> Tansu ()

-- infix alias for set
(=:)  :: ByteString -> ByteString -> Tansu ()

-- get a value, failing if it does not exist
get   :: ByteString -> Tansu ByteString

-- get a value, returning Nothing if it does not exist
getMb :: ByteString -> Tansu (Maybe ByteString)

-- remove a key and its associated value
del   :: ByteString -> Tansu ()

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

## Tansu Backends

[...]

## About the Name

A _tansu_ is a kind of
[mobile Japanese wooden cabinet](https://en.wikipedia.org/wiki/Tansu),
and the initial plan for the _tansu_ library was for it to be a convenient
API wrapper over the [Kyoto Cabinet](http://fallabs.com/kyotocabinet/)
library, but it has since become a generic wrapper over various
key-value mapping backends.
