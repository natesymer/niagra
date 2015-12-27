{-|
Module      : Data.Niagra.Block
Description : CSS declaration blocks
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

CSS declaration blocks.
-}

{-# LANGUAGE OverloadedStrings #-}
module Data.Niagra.Block
(
  -- * Types
  Block(..),
  Declaration(..),
  -- * Predicates
  isEmpty,
  -- * Builder
  buildBlock,
)
where

import Data.Niagra.Selector
import Data.Niagra.Builder

import Data.Monoid
import Data.Text (Text)

-- |A single declaration
data Declaration = Declaration Text Builder

-- |Block data structure.
data Block = DeclarationBlock Selector [Declaration]-- ^ Create a block with a declaration list for a body
           | BuilderBlock Selector Builder -- ^ create a block with a builder body

-- |Determine if a block is empty.
isEmpty :: Block -> Bool
isEmpty (DeclarationBlock _ d2) = null d2
isEmpty (BuilderBlock _ _) = False

-- |Build a string from a 'Block'
buildBlock :: Block -- ^ block to render
           -> Builder
buildBlock (BuilderBlock sel b) = mconcat [buildSelector sel, "{", b, "}"]
buildBlock (DeclarationBlock sel d) = buildBlock $ BuilderBlock sel $ buildDecls mempty d
  where
    buildDecls accum [] = accum
    buildDecls accum [Declaration p v] = accum <> fromText p <> ":" <> v
    buildDecls accum (x:xs) = buildDecls (buildDecls accum [x] <> ";") xs