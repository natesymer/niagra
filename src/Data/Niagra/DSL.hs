{-|
Module      : Data.Niagra
Description : Root module of Niagra
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Niagra DSL.
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Niagra.DSL
(
  -- * Rendering Functions
  css,
  css',
  cssBuilder,
  cssBuilder',
  -- * CSS Declaration Functions
  block,
  declaration,
  (?),
  (.=),
)
where
  
import Data.Niagra.Monad
import Data.Niagra.Selector
import Data.Niagra.Block
import Data.Niagra.Builder
  
import Control.Monad.Identity
import Data.Text (Text)
import Data.Monoid
import Data.Foldable

-- |Start a CSS declaration in monad @m@.
css :: (Monad m) => NiagraT m () -- ^ the action to render
                 -> m Text -- ^ minified CSS
css = fmap toText . cssBuilder

-- |Non-monadic vesion of 'css'.
css' :: NiagraT Identity () -> Text
css' = runIdentity . css

-- |Start a CSS declaration in monad @m@ that returns a 'Builder'.
cssBuilder :: (Monad m) => NiagraT m () -- ^ the action to render
                        -> m Builder -- ^ builder that builds CSS
cssBuilder = fmap reduce . execNiagraT Null
  where reduce = foldl' (\a b -> a <> (buildBlock b)) mempty

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
                         -> Builder -- ^ value
                         -> NiagraT m ()
declaration p v = addDeclaration $ Declaration p v

-- |Operator equivalent of 'block'.
infix 0 ?
(?) :: (Monad m) => Selector -> NiagraT m () -> NiagraT m ()
(?) = block

-- |Operator equivalent of 'declaration'.
infix 1 .=
(.=) :: (Monad m) => Text -> Builder -> NiagraT m ()
(.=) = declaration