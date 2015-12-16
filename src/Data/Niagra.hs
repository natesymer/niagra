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
  css,
  css',
  cssBuilder,
  cssBuilder',
  block,
  (?),
  declaration,
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

import Data.Either

import Control.Monad.Identity
import Data.Text.Lazy.Builder (Builder,toLazyText)
import Data.Text.Lazy (Text)

{-

TODO (in no particular order)

* wrappers around 'declaration'
* type selector parts better

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
cssBuilder = fmap (mconcat . map buildBlock) . niagraBlocks

-- |Non-monadic version of 'cssBuilder'.
cssBuilder' :: NiagraT Identity () -> Builder 
cssBuilder' = runIdentity . cssBuilder

-- |General function for defining a CSS block.
block :: (Monad m) => Selector -- ^ block's selector that
                   -> NiagraT (NiagraT m) () -- ^ the block
                   -> NiagraT m ()
block s act = niagraState act >>= writeBlocks . uncurry f
  where f decls blocks = (DeclarationBlock s decls):(map prependSel blocks)
          where prependSel (DeclarationBlock s2 d2) = DeclarationBlock (s <||> s2) d2
                prependSel (BuilderBlock s2 b2) = BuilderBlock (s <||> s2) b2 
                

-- |Operator equivalent of 'block'.
infix 0 ?
(?) :: (Monad m) => Selector -> NiagraT (NiagraT m) () -> NiagraT m ()
(?) = block

-- |Make a declaration.
declaration :: (Monad m) => Text -- ^ property
                         -> Text -- ^ value
                         -> NiagraT (NiagraT m) ()
declaration p v = writeDeclarations [Declaration p v]

-- |Operator equivalent of 'declaration'.
infix 0 .=
(.=) :: (Monad m) => Text -> Text -> NiagraT (NiagraT m) ()
(.=) = declaration