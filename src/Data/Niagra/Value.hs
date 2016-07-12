{-|
Module      : Data.Niagra.Value
Description : CSS Values
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : stable
Portability : POSIX

The 'Value' typeclass is designed to blur the distinction
between single 'Builder's, tuples of 'Builder's, and lists
of 'Builder's.

Each structure behaves differently:

* Lists are comma-separated
* Tuples are space-separated
* Plain Builders aren't modified.

-}


{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Data.Niagra.Value
(
  Value(..),
  inherit,
  initial,
  none,
  v
)
where

import Data.Monoid
import Data.Niagra.Builder

-- |Represents a CSS declaration value. 'Value' is designed to allow
-- CSS properties with many space-separated values to be declared either
-- like @[px 1, "solid"]@ or simply @px 1@.
class Value a where
  build :: a -> Builder -- ^ Build a value.
  
instance Value String where
  build = fromString
  
instance Value Builder where
  build = id
  
instance Value [Builder] where
  build = f mempty True
    where
      f a _ [] = a
      f a True (x:xs) = f x False xs
      f a False (x:xs) = f (a <> sp <> x) False xs

instance Value (Builder, Builder) where
  build (a,b) = a <> sp <> b
  
instance Value (Builder, Builder, Builder) where
  build (a,b,c) = a <> sp <> b <> sp <> c
  
instance Value (Builder, Builder, Builder, Builder) where
  build (a,b,c,d) = a <> sp <> b <> sp <> c <> sp <> d
  
instance Value (Builder, Builder, Builder, Builder, Builder) where
  build (a,b,c,d,e) = a <> sp <> b <> sp <> c <> sp <> d <> sp <> e

instance Value (Builder, Builder, Builder, Builder, Builder, Builder) where
  build (a,b,c,d,e,f) = a <> sp <> b <> sp <> c <> sp <> d <> sp <> e <> sp <> f
  
instance Value (Builder, Builder, Builder, Builder, Builder, Builder, Builder) where
  build (a,b,c,d,e,f,g) = a <> sp <> b <> sp <> c <> sp <> d <> sp <> e <> sp <> f <> sp <> g
  
instance Value (Builder, Builder, Builder, Builder, Builder, Builder, Builder, Builder) where
  build (a,b,c,d,e,f,g,h) = a <> sp <> b <> sp <> c <> sp <> d <> sp <> e <> sp <> f <> sp <> g <> sp <> h
  
-- |CSS @inherit@.
inherit :: Builder
inherit = "inherit"

-- |CSS @initial@.
initial :: Builder
initial = "initial"

-- |CSS @none@.
none :: Builder
none = "none"

v :: String -> Builder
v = fromString

{- Internal -}

sp :: Builder
sp = singleton ' '
