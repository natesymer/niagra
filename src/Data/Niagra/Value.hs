{-# LANGUAGE OverloadedStrings #-}

module Data.Niagra.Value where

import Data.Monoid
import Data.Text.Lazy.Builder (Builder,singleton)

-- |Represents a CSS declaration value. 'Value' is designed to allow
-- CSS properties with many space-separated values to be declared either
-- like @[px 1, "solid"]@ or simply @px 1@.
class Value a where
  build :: a -> Builder -- ^ Build a value.
  
instance Value Builder where
  build = id
  
instance Value [Builder] where
  build = f ""
    where
      f a [] = a
      f a [x] = a <> singleton ' ' <> build x
      f a (x:xs) = f (a <> singleton ' ' <> build x) xs
  
-- |CSS @inherit@.
inherit :: Builder
inherit = "inherit"

-- |CSS @initial@.
initial :: Builder
initial = "initial"

-- |CSS @none@.
none :: Builder
none = "none"