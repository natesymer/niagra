{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Niagra.Internal
(
  -- * Types
  Rule(..),
  Selector,
  Property,
  Value,
  -- * DSL
  css,
  rule,
  declaration,
  (?),
  renderRule
)
where
  
import Data.Niagra.Selector
import Control.Monad.Trans.Writer
  
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
  
-- a,h2{background-color:red}

css :: (Monad m) => WriterT [Rule] m () -> m [Rule]
css = execWriterT

rule :: (Monad m) => Selector -> WriterT [Declaration] (WriterT [Rule] m) () -> WriterT [Rule] m ()
rule sel act = do
  decls <- execWriterT act
  tell [Rule sel decls]

(?) :: (Monad m) => Selector -> WriterT [Declaration] (WriterT [Rule] m) () -> WriterT [Rule] m ()
(?) = rule

declaration :: (Monad m) => Property -> Value -> WriterT [Declaration] (WriterT [Rule] m) ()
declaration p v = tell [(p, v)]

renderRule :: Rule -> String
renderRule (Rule sel decls) = mconcat $ ((renderSelector sel):"{":map renderDecl decls) ++ ["}"]

renderDecl :: Declaration -> String
renderDecl (p,v) = mconcat $ [p,":",v,";"]