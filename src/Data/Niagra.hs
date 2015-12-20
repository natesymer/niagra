{-|
Module      : Data.Niagra
Description : Root module of Niagra
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Provides a basic interface for defining CSS
and rendering those blocks into strings.

Niagra produces "minified" CSS.
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Niagra
(
  -- * DSL
  -- ** Rendering Functions
  css,
  css',
  cssBuilder,
  cssBuilder',
  -- ** CSS Declaration Functions
  block,
  declaration,
  (?),
  (.=),
  -- * Modules
  module Data.Niagra.At,
  module Data.Niagra.Monad,
  module Data.Niagra.Block,
  module Data.Niagra.Selector,
  module Data.Niagra.Selector.Tags,
  module Data.Niagra.Selector.Combinators
)
where

import Data.Niagra.At
import Data.Niagra.Monad
import Data.Niagra.Block
import Data.Niagra.Selector
import Data.Niagra.Selector.Tags
import Data.Niagra.Selector.Combinators

import Control.Monad.Identity
import Data.Text.Lazy.Builder (Builder,toLazyText)
import Data.Text.Lazy (Text)

{-

TODO (in no particular order)

* wrappers around 'declaration'
* type selector parts better
* fix nested selectors

-}

-- |Start a CSS declaration in monad @m@.
css :: (Monad m) => NiagraT m () -- ^ the action to render
                 -> m Text -- ^ minified CSS
css = fmap toLazyText . cssBuilder

-- |Non-monadic vesion of 'css'.
css' :: NiagraT Identity () -> Text
css' = runIdentity . css

-- |Start a CSS declaration in monad @m@ that returns a 'Builder'.
cssBuilder :: (Monad m) => NiagraT m () -- ^ the action to render
                        -> m Builder -- ^ builder that builds CSS
cssBuilder = fmap (mconcat . map buildBlock) . execNiagraT Null

-- |Non-monadic version of 'cssBuilder'.
cssBuilder' :: NiagraT Identity () -> Builder 
cssBuilder' = runIdentity . cssBuilder

-- |Defines a CSS block.
block :: (Monad m) => Selector -- ^ block's selector that
                   -> NiagraT m () -- ^ action declaring the block
                   -> NiagraT m ()
block sel declarator = do
  withNewScope sel $ do
    declarator
    getCurrentBlock >>= addBlock

-- |Make a declaration.
declaration :: (Monad m) => Text -- ^ property
                         -> Text -- ^ value
                         -> NiagraT m ()
declaration p v = addDeclaration $ Declaration p v

-- |Operator equivalent of 'block'.
infix 0 ?
(?) :: (Monad m) => Selector -> NiagraT m () -> NiagraT m ()
(?) = block

-- |Operator equivalent of 'declaration'.
infix 1 .=
(.=) :: (Monad m) => Text -> Text -> NiagraT m ()
(.=) = declaration