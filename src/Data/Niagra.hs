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
  (.=)
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
* pseudo* syntax

-}

-- |Start a CSS declaration in monad @m@.
css :: (Monad m) => WriterT [Either Declaration Block] m () -> m BL.ByteString
css = fmap toLazyByteString . cssBuilder

-- |Start a CSS declaration in the 'Identity' monad. Returns
-- resulting CSS outside of the 'Identity' monad.
css' :: WriterT [Either Declaration Block] Identity () -> BL.ByteString
css' = runIdentity . css

-- |Start a CSS declaration in monad @m@ that returns a 'Builder'
cssBuilder :: (Monad m) => WriterT [Either Declaration Block] m () -> m Builder
cssBuilder = fmap (mconcat . map build . rights) . execWriterT

cssBuilder' :: WriterT [Either Declaration Block] Identity () -> Builder
cssBuilder' = runIdentity . cssBuilder

-- |General function for defining a CSS block
block :: (Monad m) => Selector -- ^ the 'Buildable' that names the block
                   -> WriterT [Either Declaration Block] (WriterT [Either Declaration Block] m) () -- ^ the block
                   -> WriterT [Either Declaration Block] m ()
block b act = execWriterT act >>= f b
  where
    f b res = tell $ map Right $ blk:(map (mappend blk) (rights res))
      where blk = Block b $ lefts res

-- |Operator equivalent of 'block'.
infix 2 ?
(?) :: (Monad m) => Selector -- ^ the 'Buildable' that names the block
                 -> WriterT [Either Declaration Block] (WriterT [Either Declaration Block] m) () -- ^ the block
                 -> WriterT [Either Declaration Block] m ()
(?) = block

-- |Make a declaration.
declaration :: (Monad m) => String -- ^ property
                         -> String -- ^ value
                         -> WriterT [Either Declaration Block] (WriterT [Either Declaration Block] m) ()
declaration p v = tell [Left $ Declaration p v]

-- |Operator equivalent of 'declaration'
infix 2 .=
(.=) :: (Monad m) => String -> String -> WriterT [Either Declaration Block] (WriterT [Either Declaration Block] m) ()
(.=) = declaration
