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

import Data.Niagra.Builder.Buffer
import Data.Niagra.AccumulatorT

import Control.Monad
import Control.Monad.ST

import Data.Word
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.String as STR

import Data.Text.Internal (Text(..))

-- A type alias for the 'AccumulatorT' used to accumulate
-- builder strings
type BuilderAccum s a = AccumulatorT Text (Buffer s) (ST s) a

-- |Builder data structure. Builders accumulate 'Char's &
-- 'Text's.
data Builder = EmptyBuilder | Builder (forall s. BuilderAccum s ())

runBuilder :: Builder -> ST s [Text]
runBuilder EmptyBuilder = return []
runBuilder (Builder acc) = do
  (_,sq,_) <- run
  return $ toList sq
  where
    run = evalAccumulatorT acc' freezeBuffer mkIncomp-- runAccumulatorT acc' freezeBuffer mkIncomp S.empty
    mkIncomp = newBuffer 128
    acc' = do
      acc
      l <- bufferLength <$> getIncomplete
      when (l > 0) $ do
        incomplete shrinkBuffer
        complete

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
singleton c = Builder $ appendChar c

-- | O(1) create a 'Builder' from a 'String'.
fromString :: String -> Builder
fromString [] = empty
fromString s = Builder $ mapM_ appendChar s

-- | O(1) create a 'Builder' from a 'Text'.
fromText :: Text -> Builder
fromText t = Builder $ appendText t

-- | O(1) create a 'Builder' from a lazy 'Text'.
fromLazyText :: TL.Text -> Builder
fromLazyText tl = Builder $ mapM_ appendText $ TL.toChunks tl

-- | O(1) append two 'Builder's.
appendBuilder :: Builder -> Builder -> Builder
appendBuilder EmptyBuilder a = a
appendBuilder a EmptyBuilder = a
appendBuilder (Builder a) (Builder b) = Builder $ a >> b
  
-- | O(n) Turn a 'Builder' into a 'Text'. While 'singleton', 'fromString',
-- 'fromText', 'empty', and 'appendBuilder' don't do any direct processing,
-- /the monadic action they construct gets evaluated here/. @n@ is the length of
-- the accumulated data to be built into a 'Text'
toText :: Builder -> Text
toText = TL.toStrict . toLazyText 

-- |Lazy version of 'toText'.
toLazyText :: Builder -> TL.Text
toLazyText b = runST $ TL.fromChunks <$> runBuilder b

{-

these functions use the primive functions
inside 'AccumulatorT' to do the heavy lifting
(no pun intended) behind 'Builder'.

-}

-- |Safely append a Word16 to the incomplete Buffer.
builderAppendWord16 :: Word16 -> BuilderAccum s ()
builderAppendWord16 w = do
  remain <- bufferRemaining <$> getIncomplete
  if remain == 0
    then do
      complete
      builderAppendWord16 w
    else incomplete $ flip bufferAppendWord16 w

-- |Append a char to the end of a Builder's accumulation.
appendChar :: Char -> BuilderAccum s ()
appendChar = either writeSingle writeDouble . charToWord16
  where
    writeSingle = builderAppendWord16
    writeDouble (lo,hi) = do
      builderAppendWord16 lo
      builderAppendWord16 hi

-- |Append a 'Text' to the end of a Builder's accumulation.
appendText :: Text -> BuilderAccum s ()
appendText t@(Text _ _ tl) = do
  remain <- bufferRemaining <$> getIncomplete
  if remain > tl
    then incomplete (bufferAppendText t tl)
    else do
      incomplete (bufferAppendText t remain)
      complete
      appendText $ offsetText t remain