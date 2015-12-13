module Data.Niagra.Rule
(
  Rule(..),
  Declaration(..),
  Property(..),
  Value(..)
)
where

import Data.Niagra.Selector

data Rule = Rule {
  cssSelector :: Selector, -- TODO: equivalentcy tests for selectors (other than (==))
  cssDeclarationBlock :: [Declaration]
} deriving (Eq,Show)

type Declaration = (Property, Value)
type Property = String
type Value = String
  
-- | Convert a data structure to a Rule
class ToRule a where
  toRule :: a -> Rule