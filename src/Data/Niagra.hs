module Data.Niagra
(
  -- * Modules
  module Data.Niagra.Block,
  module Data.Niagra.Selector,
  module Data.Niagra.Selector.Tags,
  -- * DSL
  css,
  css',
  cssBuilder,
  cssBuilder',
  block,
  (?),
  declaration,
  (.=),
  media
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

* wrappers around 'declaration'
* more operators
* operator precedence
-}

-- |Start a CSS declaration in monad @m@.
css :: (Monad m) => NiagraT m () -> m Text
css = fmap toLazyText . cssBuilder

-- |Start a CSS declaration in the 'Identity' monad. Returns
-- resulting CSS outside of the 'Identity' monad.
css' :: NiagraT Identity () -> Text
css' = runIdentity . css

-- |Start a CSS declaration in monad @m@ that returns a 'Builder'
cssBuilder :: (Monad m) => NiagraT m () -> m Builder
cssBuilder = fmap (mconcat . map buildBlock . rights) . execNiagraT

-- cssBuilder' :: WriterT [Either Declaration Block] Identity () -> Builder
cssBuilder' :: NiagraT Identity () -> Builder
cssBuilder' = runIdentity . cssBuilder

-- |General function for defining a CSS block
block :: (Monad m) => Selector -- ^ block's selector that
                   -> NiagraT (NiagraT m) () -- ^ the block
                   -> NiagraT m ()
block b act = execNiagraT act >>= writeBlocks . uncurry f . partitionEithers
  where f l = (:) (DeclarationBlock b l) . map appendSel
          where appendSel (DeclarationBlock b2 d2) = DeclarationBlock (b <||> b2) d2

-- |Operator equivalent of 'block'.
infix 2 ?
(?) :: (Monad m) => Selector -> NiagraT (NiagraT m) () -> NiagraT m ()
(?) = block

-- |Make a declaration.
declaration :: (Monad m) => String -- ^ property
                         -> String -- ^ value
                         -> NiagraT (NiagraT m) ()
declaration p v = writeDeclarations [Declaration p v]

-- |Operator equivalent of 'declaration'.
infix 2 .=
(.=) :: (Monad m) => String -> String -> NiagraT (NiagraT m) ()
(.=) = declaration

-- |A @media query.
media :: (Monad m) => String
                   -> NiagraT (NiagraT m) () -- ^ content of the @media query
                   -> NiagraT m ()
media str act = cssBuilder act >>= writeBlocks . f
  where f b = [BuilderBlock sel b]
        sel = Raw $ "@media " ++ str