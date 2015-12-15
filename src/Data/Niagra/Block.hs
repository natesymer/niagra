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
  blockBuildable :: Selector,
  blockDeclarationBlock :: Either Builder [Declaration] -- Allow greater flexibility
}

instance Buildable Block where
  build (Block b decls) = build b <> char8 '{' <> either id buildDecls decls <> char8 '}'
    where
      buildDecls [] = mempty
      buildDecls [Declaration p v] = string8 p <> char8 ':' <> string8 v
      buildDecls ((Declaration p v):xs) = string8 p <> char8 ':' <> string8 v <> char8 ';' <> buildDecls xs

data Declaration = Declaration {
  declarationProperty :: String,
  declarationValue :: String
} deriving (Eq, Show)