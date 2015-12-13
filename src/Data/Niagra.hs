module Data.Niagra
(
  -- * Modules
  module Data.Niagra.Rule,
  module Data.Niagra.Selector,
  module Data.Niagra.Selector.Tags,
  module Data.Niagra.Selector.Attribute,
  -- * DSL
  css,
  css',
  cssBuilder,
  cssBuilder',
  fontFace,
  block,
  (?),
  declaration,
  (.=)
)
where

import Data.Niagra.Buildable
import Data.Niagra.Rule
import Data.Niagra.Rule.FontFace
import Data.Niagra.Selector
import Data.Niagra.Selector.Tags
import Data.Niagra.Selector.Attribute

import Control.Monad.Trans.Writer
import Control.Monad.Identity
import Data.ByteString.Builder (Builder,toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as BL

{-
TODO

2. wrappers around 'declaration'
3. nested selectors (by Attribute, PseudoType, and PseudoClass, see Subbable typeclass)
4. more operators
5. operator precedence
6. pseudo* syntax
7. guarantee correctness

-}

-- |Start a CSS declaration in monad @m@.
css :: (Monad m) => WriterT [Builder] m () -> m BL.ByteString
css = fmap toLazyByteString . cssBuilder

-- |Start a CSS declaration in the 'Identity' monad. Returns
-- resulting CSS outside of the 'Identity' monad.
css' :: WriterT [Builder] Identity () -> BL.ByteString
css' = runIdentity . css

-- |Start a CSS declaration in monad @m@ that returns a 'Builder'
cssBuilder :: (Monad m) => WriterT [Builder] m () -> m Builder
cssBuilder = fmap mconcat . execWriterT

cssBuilder' :: WriterT [Builder] Identity () -> Builder
cssBuilder' = runIdentity . cssBuilder

-- |General function for defining a CSS block
block :: (Monad m, Buildable b) => b -- ^ the 'Buildable' that names the block
                                -> WriterT [Declaration] (WriterT [Builder] m) () -- ^ the block
                                -> WriterT [Builder] m ()
block b act = do
  decls <- execWriterT act
  tell [build $ Rule b decls]
  
-- |Operator equivalent of 'cssBlock'.
infix 2 ?
(?) :: (Monad m, Buildable b) => b -> WriterT [Declaration] (WriterT [Builder] m) () -> WriterT [Builder] m ()
(?) = block

-- |Define a font-face
fontFace :: (Monad m) => WriterT [Declaration] (WriterT [Builder] m) () -- ^ Declaration block action
                      -> WriterT [Builder] m ()
fontFace = block FontFace

-- |Make a declaration.
declaration :: (Monad m) => Property -- ^ property
                         -> Value -- ^ value
                         -> WriterT [Declaration] (WriterT [Builder] m) ()
declaration p v = tell [(p, v)]

-- |Operator equivalent of 'declaration'
infix 2 .=
(.=) :: (Monad m) => Property -> Value -> WriterT [Declaration] (WriterT [Builder] m) ()
(.=) = declaration
