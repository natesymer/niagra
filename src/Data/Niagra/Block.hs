module Data.Niagra.Block
(
  Block(..),
  Declaration(..)
)
where

import Data.Niagra.Buildable

import Data.Monoid
import Data.Niagra.Selector
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BL

data Block = Block {
  blockBuildable :: Selector, -- TODO: equivalentcy tests for selectors (other than (==))
  blockDeclarationBlock :: [Declaration]
} deriving (Eq,Show)

instance Buildable Block where
  build (Block b decls) = build b <> char8 '{' <> buildDecls decls <> char8 '}'
    where
      buildDecls [] = mempty
      buildDecls [Declaration p v] = string8 p <> char8 ':' <> string8 v
      buildDecls ((Declaration p v):xs) = string8 p <> char8 ':' <> string8 v <> char8 ';' <> buildDecls xs

instance Monoid Block where
  mempty = Block mempty []
  mappend (Block b d) (Block b2 d2) = Block (Descendant b b2) (mappend d d2)

data Declaration = Declaration {
  declarationProperty :: String,
  declarationValue :: String
} deriving (Eq, Show)