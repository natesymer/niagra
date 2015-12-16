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
  media,
  fontFace,
  -- * Modules
  module Data.Niagra.Monad,
  module Data.Niagra.Block,
  module Data.Niagra.Selector,
  module Data.Niagra.Selector.Tags
)
where

import Data.Niagra.Monad
import Data.Niagra.Block
import Data.Niagra.Selector
import Data.Niagra.Selector.Tags

import Data.Either

import Control.Monad.Identity
import Data.Text.Lazy.Builder (Builder,toLazyText)
import Data.Text.Lazy (Text)

{-
TODO (in no particular order)

* NiagraT return tuple: (CSS, NiagraT value)

* wrappers around 'declaration'
* more operators
* operator precedence
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
block b act = niagraState act >>= writeBlocks . uncurry f
  where f l = (:) (DeclarationBlock b l) . map appendSel
          where appendSel (DeclarationBlock b2 d2) = DeclarationBlock (b <||> b2) d2

-- |Operator equivalent of 'block'.
infix 2 ?
(?) :: (Monad m) => Selector -> NiagraT (NiagraT m) () -> NiagraT m ()
(?) = block

-- |Make a declaration.
declaration :: (Monad m) => Text -- ^ property
                         -> Text -- ^ value
                         -> NiagraT (NiagraT m) ()
declaration p v = writeDeclarations [Declaration p v]

-- |Operator equivalent of 'declaration'.
infix 2 .=
(.=) :: (Monad m) => Text -> Text -> NiagraT (NiagraT m) ()
(.=) = declaration

-- |A @media query.
media :: (Monad m) => Text
                   -> NiagraT (NiagraT m) () -- ^ content of the @media query
                   -> NiagraT m ()
media str act = cssBuilder act >>= writeBlocks . f
  where f b = [BuilderBlock sel b]
        sel = Raw $ mappend "@media " str
        
-- |A @font-face
fontFace :: (Monad m) => NiagraT (NiagraT m) () -- ^ content of the @font-face
                      -> NiagraT m ()
fontFace act = niagraDeclarations act >>= writeBlocks . f
  where f b = [DeclarationBlock "@font-face" b]