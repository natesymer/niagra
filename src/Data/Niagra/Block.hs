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
  Block(..),
  buildBlock,
  appendDeclaration
)
where

import Data.Niagra.Selector
import Data.Niagra.Builder

import Data.Monoid
import Data.Text (Text)

-- |Block data structure.
data Block = Block Selector Builder

-- |Add a declaration to a 'Block'.
appendDeclaration :: Block -> Text -> Builder -> Block
appendDeclaration (Block s EmptyBuilder) k v = Block s b'
  where b' = fromText k <> singleton ':' <> v
appendDeclaration (Block s b) k v = Block s b'
  where b' = b <> singleton ';' <> fromText k <> singleton ':' <> v
  
-- |Build a block
buildBlock :: Block -> Builder
buildBlock (Block s EmptyBuilder) = EmptyBuilder
buildBlock (Block s b) = buildSelector s <> singleton '{' <> b <> singleton '}'