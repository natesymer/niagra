{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Data.Niagra.Buildable
import Data.Niagra.Block
import Data.Niagra.Selector
import Data.Niagra.Selector.Tags

import Data.Either

import Control.Monad.Trans.Writer
import Control.Monad.Identity
import Data.ByteString.Builder (Builder,toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as BL

{-
TODO (in no particular order)

* wrappers around 'declaration'
* more operators
* operator precedence

-}
--------------------------------------------------------------------------------
newtype NiagraT m a = NiagraT (WriterT [Either Declaration Block] m a)
  deriving (Functor, Applicative, Monad)
  
execNiagraT :: (Monad m) => NiagraT m a -> m [Either Declaration Block]
execNiagraT (NiagraT w) = execWriterT w

writeBlocks :: (Monad m) => [Block] -> NiagraT m ()
writeBlocks = NiagraT . tell . map Right

writeDeclarations :: (Monad m) => [Declaration] -> NiagraT m ()
writeDeclarations = NiagraT . tell . map Left

-- |Start a CSS declaration in monad @m@.
css :: (Monad m) => NiagraT m () -> m BL.ByteString
css = fmap toLazyByteString . cssBuilder

-- |Start a CSS declaration in the 'Identity' monad. Returns
-- resulting CSS outside of the 'Identity' monad.
css' :: NiagraT Identity () -> BL.ByteString
css' = runIdentity . css

-- |Start a CSS declaration in monad @m@ that returns a 'Builder'
cssBuilder :: (Monad m) => NiagraT m () -> m Builder
cssBuilder = fmap (mconcat . map build . rights) . execNiagraT

-- cssBuilder' :: WriterT [Either Declaration Block] Identity () -> Builder
cssBuilder' :: NiagraT Identity () -> Builder
cssBuilder' = runIdentity . cssBuilder

-- |General function for defining a CSS block
block :: (Monad m) => Selector -- ^ block's selector that
                   -> NiagraT (NiagraT m) () -- ^ the block
                   -> NiagraT m ()
block b act = execNiagraT act >>= writeBlocks . uncurry f . partitionEithers
  where f l = (:) (Block b $ Right l) . map appendSel
          where appendSel (Block b2 d2) = Block (b <||> b2) d2

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

-- |A media query
media :: (Monad m) => String
                   -> NiagraT (NiagraT m) () -- ^ content of the @media query
                   -> NiagraT m ()
media str act = cssBuilder act >>= writeBlocks . f
  where f b = [Block sel $ Left b]
        sel = Raw $ "@media " ++ str