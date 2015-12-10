{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Niagra.Internal
(
  -- * Types
  Rule(..),
  Selector,
  Property,
  Value,
)
where
  
import Data.Niagra.Selector
  
data Rule = Rule {
  cssSelectors :: [Selector], -- TODO: equivalentcy tests for selectors (other than (==))
  cssDeclarationBlock :: [Declaration]
} deriving (Eq,Show)

type Declaration = (Property, Value)
type Property = String
type Value = String

-- | Convert a list of @a@ to CSS
class ToRule a => ToCSS a where
  toCSS :: [a] -> [Rule]
  toCSS = map toRule
  
-- | Convert a data structure to a Rule
class ToRule a where
  toRule :: a -> Rule