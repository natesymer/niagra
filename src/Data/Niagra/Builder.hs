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
of bytes at most once: from a 'Text','String', or 'Char'
to a buffer.

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
import qualified Data.Text.Lazy as TL

import Data.Foldable
import qualified Data.String as STR

import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as S

-- TODO:
-- * Faster string-centric routine
-- * Define Builder in terms of AccumulatorT

data Builder = EmptyBuilder | Builder {
  runBuilder :: forall s. ((Buffer s, Seq Text) -> ST s (Buffer s, Seq Text))
             -> (Buffer s, Seq Text)
             -> ST s (Buffer s, Seq Text)
}

evalBuilder :: Builder -> ST s [Text]
evalBuilder EmptyBuilder = return []
evalBuilder (Builder f) = do
  (h,t) <- unsafeNewBuffer >>= f return . (,S.empty)
  flushed <- bufferToText h
  return $ toList $ t |> flushed

instance Monoid Builder where
  mempty  = empty
  mappend = appendBuilder
  
instance STR.IsString Builder where
  -- creating a builder from a lazy 'Text' is faster
  -- than creating one from a 'String'
  fromString [c] = singleton c
  fromString s = fromLazyText $ TL.pack s
  
-- | O(1) determine if a builder is empty.
isEmpty :: Builder -> Bool
isEmpty EmptyBuilder = True
isEmpty _ = False

-- | O(1) create an empty 'Builder'
{-# INLINE empty #-}
empty :: Builder
empty = EmptyBuilder

-- biggest overhead comes from the binding operator
-- | O(1) create a 'Builder' from a single 'Char'.
singleton :: Char -> Builder
singleton c = Builder $ \f tup -> snocVec c tup >>= f

-- | O(1) create a 'Builder' from a 'String'.
fromString :: String -> Builder
fromString [] = empty
fromString [x] = singleton x
fromString s = Builder $ \f tup -> foldlM (flip snocVec) tup s >>= f

-- | O(1) create a 'Builder' from a 'Text'.
fromText :: Text -> Builder
fromText t = Builder $ \f tup -> appendVec t tup >>= f

-- | O(1) create a 'Builder' from a lazy 'Text'.
fromLazyText :: TL.Text -> Builder
fromLazyText t = Builder $ \f tup -> foldlM (flip appendVec) tup (TL.toChunks t) >>= f

-- | O(1) append two 'Builder's.
appendBuilder :: Builder -> Builder -> Builder
appendBuilder EmptyBuilder a = a
appendBuilder a EmptyBuilder = a
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