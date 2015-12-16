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
  -- * Builder
  buildBlock,
  -- * DSL
  
)
where

import Data.Niagra.Selector

import Data.Monoid
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder

-- |A single declaration
data Declaration = Declaration Text Text

-- |Block data structure.
data Block = DeclarationBlock Selector [Declaration] -- ^ Create a block with a declaration list for a body
           | BuilderBlock Selector Builder -- ^ create a block with a builder body

-- |Build a string from a 'Block'
buildBlock :: Block -- ^ block to render
           -> Builder
buildBlock (BuilderBlock sel b) = mconcat [buildSelector sel, "{", b, "}"]
buildBlock (DeclarationBlock sel d) = buildBlock $ BuilderBlock sel $ buildDecls mempty d
  where
    pair p v = fromLazyText p <> ":" <> fromLazyText v
    buildDecls accum [] = accum
    buildDecls accum [Declaration p v] = accum <> pair p v
    buildDecls accum (x:xs) = buildDecls (buildDecls accum [x] <> ";") xs