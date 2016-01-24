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
  -- * CSS Crunching
  css,
  css',
  cssBuilder,
  cssBuilder',
  -- * Modules
  module Data.Niagra.At,
  module Data.Niagra.Block,
  module Data.Niagra.Builder,
  module Data.Niagra.Builder.Numbers,
  module Data.Niagra.Monad,
  module Data.Niagra.Properties,
  module Data.Niagra.Selector,
  module Data.Niagra.Selector.Tags,
  module Data.Niagra.Selector.Combinators,
  module Data.Niagra.Value,
)
where

import Data.Niagra.At
import Data.Niagra.Block
import Data.Niagra.Builder
import Data.Niagra.Builder.Numbers
import Data.Niagra.Monad
import Data.Niagra.Properties
import Data.Niagra.Selector
import Data.Niagra.Selector.Tags
import Data.Niagra.Selector.Combinators
import Data.Niagra.Value

import Data.Text (Text)

-- |Start a CSS declaration in monad @m@.
css :: (Monad m) => NiagraT m () -- ^ the action to render
                 -> m Text -- ^ minified CSS
css = fmap toText . cssBuilder

-- |Non-monadic vesion of 'css'.
css' :: Niagra () -> Text
css' = toText . cssBuilder'

-- |Start a CSS declaration in monad @m@ that returns a 'Builder'.
cssBuilder :: (Monad m) => NiagraT m () -- ^ the action to render
                        -> m Builder -- ^ builder that builds CSS
cssBuilder = fmap (foldMap buildBlock) . runNiagraT

-- |Non-monadic version of 'cssBuilder'.
cssBuilder' :: Niagra () -> Builder 
cssBuilder' = foldMap buildBlock . runNiagra