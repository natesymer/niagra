module Data.Niagra.Block
(
  Block(..),
  Declaration(..),
  buildBlock
)
where

import Data.Monoid
import Data.Niagra.Selector
import Data.Text.Lazy.Builder

data Declaration = Declaration {
  declarationProperty :: String,
  declarationValue :: String
} deriving (Eq, Show)

data Block = DeclarationBlock Selector [Declaration]
           | BuilderBlock Selector Builder

buildBlock :: Block -> Builder
buildBlock (BuilderBlock sel b) = mconcat [buildSelector sel, singleton '{', b, singleton '}']
buildBlock (DeclarationBlock sel d) = buildBlock $ BuilderBlock sel $ buildDecls mempty d
  where
    buildDecls accum [] = accum
    buildDecls accum [Declaration p v] = mconcat [accum, fromString p, singleton ':', fromString v]
    buildDecls accum ((Declaration p v):xs) = buildDecls (mconcat [accum, fromString p, singleton ':', fromString v, singleton ';']) xs