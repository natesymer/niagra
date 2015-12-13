{-# LANGUAGE GADTSyntax #-}
module Data.Niagra.Rule
(
  Rule(..),
  Declaration(..),
  Property(..),
  Value(..)
)
where

import Data.Niagra.Buildable

import Data.Monoid
import Data.Niagra.Selector
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BL

data Rule a = Rule {
  ruleBuildable :: a, -- TODO: equivalentcy tests for selectors (other than (==))
  ruleDeclarationBlock :: [Declaration]
} deriving (Eq,Show)

instance (Buildable a) => Buildable (Rule a) where
  build (Rule b decls) = build b <> char8 '{' <> buildDecls decls <> char8 '}'
    where
      buildDecls [] = mempty
      buildDecls [(p,v)] = string8 p <> char8 ':' <> string8 v
      buildDecls ((p,v):xs) = string8 p <> char8 ':' <> string8 v <> char8 ';' <> buildDecls xs

type Declaration = (Property, Value)
type Property = String
type Value = String
  
-- | Convert a data structure to a Rule
class ToRule a where
  toRule :: (Buildable b) => a -> Rule b