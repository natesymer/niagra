module Data.Niagra
(
  -- * Modules
  module Data.Niagra.Selector,
  module Data.Niagra.Selector.Tags,
  module Data.Niagra.Selector.Attribute,
  -- * DSL
  css,
  css',
  rule,
  rules,
  declaration,
  (?),
  renderRule
)
where

import Data.Niagra.Rule
import Data.Niagra.Selector
import Data.Niagra.Selector.Tags
import Data.Niagra.Selector.Attribute

import Control.Monad.Trans.Writer
import Control.Monad.Identity

{-
TODO

1. @font-face
2. wrappers around 'declaration'
3. finish attributes
4. more operators
5. operator precedence

-}
  
-- |Start a CSS declaration in monad @m@.
css :: (Monad m) => WriterT [Rule] m () -> m String
css = fmap (mconcat . map renderRule) . rules

-- |Start a CSS declaration in the 'Identity' monad. Returns
-- resulting CSS outside of the 'Identity' monad.
css' :: WriterT [Rule] Identity () -> String
css' = runIdentity . css

-- |Evaluate a rule-generating action
rules :: (Monad m) => WriterT [Rule] m () -> m [Rule]
rules = execWriterT

-- |Define a rule
rule :: (Monad m) => Selector -- ^ Selector to which a declaration block pertains
                  -> WriterT [Declaration] (WriterT [Rule] m) () -- ^ Declaration block action
                  -> WriterT [Rule] m ()
rule sel act = do
  decls <- execWriterT act
  tell [Rule sel decls]

-- |Operator equivalent of 'rule'.
infix 2 ?
(?) :: (Monad m) => Selector -> WriterT [Declaration] (WriterT [Rule] m) () -> WriterT [Rule] m ()
(?) = rule

-- |Make a declaration.
declaration :: (Monad m) => Property -- ^ property
                         -> Value -- ^ value
                         -> WriterT [Declaration] (WriterT [Rule] m) ()
declaration p v = tell [(p, v)]

renderRule :: Rule -> String
renderRule (Rule sel decls) = mconcat $ ((renderSelector sel):"{":map renderDecl decls) ++ ["}"]

renderDecl :: Declaration -> String
renderDecl (p,v) = mconcat $ [p,":",v,";"]