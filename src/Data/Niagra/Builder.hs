{-|
Module      : Data.Niagra.Builder
Description : Lazy/Eager Text builder
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

'Text' builder based on 'AccumulatorT'. Roughly
based on 'Data.Text.Lazy.Builder'.

-}

{-# LANGUAGE Rank2Types #-}
module Data.Niagra.Builder
(
  Builder(..),
  runBuilder,
  singleton,
  fromString,
  fromText,
  fromLazyText,
  toText,
  toLazyText,
  flush
)
where

import Data.Niagra.Builder.Buffer
import Data.Niagra.AccumulatorT

import Data.Word
import Control.Monad
import Control.Monad.ST

import qualified Data.Text.Lazy as TL
import qualified Data.String as STR

import Data.Text.Internal (Text(..))
import qualified Data.Text.Internal.Lazy as TL (Text(..),chunk)

-- TODO: ensure this is lazy

-- |Builder data structure. Builders accumulate 'Char's & 'Text's.
data Builder = EmptyBuilder | Builder (forall s. AccumulatorT Text (Buffer s) (ST s) ())

runBuilder :: Builder -> TL.Text
runBuilder EmptyBuilder = TL.Empty
runBuilder b = runST $ fmap f $ run (b `mappend` flush)
  where f (_,v,_) = foldr TL.chunk TL.Empty v
        run (Builder b) = evalAccumulatorT b freezeBuffer $ newBuffer 128

instance Monoid Builder where
  mempty = EmptyBuilder
  mappend EmptyBuilder a = a
  mappend a EmptyBuilder = a
  mappend (Builder a) (Builder b) = Builder $ a >> b
  
instance STR.IsString Builder where
  fromString = fromString

-- | O(1) determine if a builder is empty.
isEmpty :: Builder -> Bool
isEmpty EmptyBuilder = True
isEmpty _ = False

-- | O(1) create a 'Builder' from a single 'Char'.
singleton :: Char -> Builder
singleton c = Builder $ appendChar c

-- | O(1) create a 'Builder' from a 'String'.
fromString :: String -> Builder
fromString [] = EmptyBuilder
fromString s = Builder $ mapM_ appendChar s

-- | O(1) create a 'Builder' from a 'Text'.
fromText :: Text -> Builder
fromText (Text _ _ 0) = EmptyBuilder
fromText t = Builder $ appendText t

-- | O(1) create a 'Builder' from a lazy 'Text'.
fromLazyText :: TL.Text -> Builder
fromLazyText = flip f EmptyBuilder
  where
    f TL.Empty acc = acc
    f (TL.Chunk c cs) acc = f cs $ mappend acc $ Builder $ appendText c

-- | O(n) Turn a 'Builder' into a 'Text'. While 'singleton', 'fromString',
-- 'fromText', 'empty', and 'appendBuilder' don't do any direct processing,
-- /the monadic action they construct gets evaluated here/. @n@ is the length of
-- the accumulated data to be built into a 'Text'
{-# INLINE toText #-}
toText :: Builder -> Text
toText = TL.toStrict . runBuilder

-- |Lazy version of 'toText'.
{-# INLINE toLazyText #-}
toLazyText :: Builder -> TL.Text
toLazyText = runBuilder

flush :: Builder
flush = Builder $ do
  l <- bufferLength <$> getIncomplete
  when (l > 0) $ do
    incomplete shrinkBuffer
    complete

{-

these functions use the primive functions
inside 'AccumulatorT' to do the heavy lifting
(no pun intended) behind 'Builder'.

-}

-- |Safely append a 'Word16' to the end of a 'Builder''s accumulation.
builderAppendWord16 :: Word16 -> AccumulatorT Text (Buffer s) (ST s) ()
builderAppendWord16 w = do
  remain <- bufferRemaining <$> getIncomplete
  when (remain == 0) complete
  incomplete $ bufferAppendWord16 w

-- |Append a char to the end of a 'Builder''s accumulation.
appendChar :: Char -> AccumulatorT Text (Buffer s) (ST s) ()
appendChar = either builderAppendWord16 writeDouble . charToWord16
  where writeDouble (lo,hi) = do
          builderAppendWord16 lo
          builderAppendWord16 hi

-- |Append a 'Text' to the end of a 'Builder''s accumulation.
appendText :: Text -> AccumulatorT Text (Buffer s) (ST s) ()
appendText t@(Text _ _ tl) = do
  remain <- bufferRemaining <$> getIncomplete
  if remain > tl
    then incomplete (bufferAppendText t tl)
    else do
      incomplete (bufferAppendText t remain)
      complete
      appendText $ offsetText t remain