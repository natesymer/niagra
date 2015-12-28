{-|
Module      : Data.Niagra.Builder
Description : Lazy/Eager Text builder
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Lazy/Eager 'Text' builder built on top of
text technologies using better data structures
than the original 'Data.Text.Lazy.Builder'.

It uses the 'Seq' data structure to hold chunks
rather than a List, because 'Seq's have O(1)
access to either end of the structure. This is
crucial for accumulating chunks in as little time
possible.

Furthermore, this builder only copies a given sequence
of bytes at most twice:

1. from 'Text','String', or 'Char' to buffer.
2. when creating an eager 'Text'

-}

{-# LANGUAGE Rank2Types, TupleSections #-}
module Data.Niagra.Builder
(
  Builder(..),
  singleton,
  fromString,
  fromText,
  fromLazyText,
  toText,
  toLazyText
)
where

import Data.Niagra.Builder.Internal

import Control.Monad.ST

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Data.Foldable
import qualified Data.String as STR

import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as S

-- TODO: Fix buffer issues with strings

-- |Wrapper around a function that applies changes
-- to a sequence of mutable buffers in 'ST'.
data Builder = Builder {
  runBuilder :: forall s. ((Buffer s, Seq Text) -> ST s (Buffer s, Seq Text))
             -> (Buffer s, Seq Text)
             -> ST s (Buffer s, Seq Text)
}

evalBuilder :: Builder -> ST s [Text]
evalBuilder (Builder f) = do
  (h,t) <- newPinnedBuffer >>= f return . (,S.empty)
  flushed <- bufferToText h
  return $ toList $ t |> flushed

instance Monoid Builder where
  mempty  = empty
  mappend = appendBuilder
  
instance STR.IsString Builder where
  fromString = fromString

-- | O(1) create an empty 'Builder'
empty :: Builder
empty = Builder $ \f v -> f v

-- biggest overhead comes from the binding operator
-- | O(1) create a 'Builder' from a single 'Char'.
singleton :: Char -> Builder
singleton c = Builder $ \f tup -> snocVec c tup >>= f

-- | O(1) create a 'Builder' from a 'String'.
fromString :: String -> Builder
-- fromString [] = empty
fromString s = Builder $ \f tup -> foldlM (flip snocVec) tup s >>= f
-- fromString = fromText . T.pack

-- | O(1) create a 'Builder' from a 'Text'.
fromText :: Text -> Builder
fromText t = Builder $ \f tup -> appendVec t tup >>= f

-- | O(1) create a 'Builder' from a lazy 'Text'.
fromLazyText :: TL.Text -> Builder
fromLazyText = mconcat . map fromText . TL.toChunks

-- | O(1) append two 'Builder's.
appendBuilder :: Builder -> Builder -> Builder
appendBuilder (Builder a) (Builder b) = Builder $ a . b
  
-- | O(n) Turn a 'Builder' into a 'Text'. While 'singleton', 'fromString',
-- 'fromText', 'empty', and 'appendBuilder' don't do any direct processing,
-- /the function they construct gets evaluated here/. @n@ is the length of
-- the accumulated data to be built into a 'Text'
toText :: Builder -> Text
toText = TL.toStrict . toLazyText 

-- |Lazy version of 'toText'.
toLazyText :: Builder -> TL.Text
toLazyText b = runST $ TL.fromChunks <$> evalBuilder b