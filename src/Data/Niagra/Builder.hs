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

{-# LANGUAGE Rank2Types, MagicHash, BangPatterns, UnboxedTuples, TupleSections #-}
module Data.Niagra.Builder
(
  Builder(..),
  singleton,
  fromString,
  fromText,
  fromLazyText,
  toText,
  toLazyText,
  decimal,
  hexadecimal,
  realFloat
)
where

import Control.Monad.ST

import GHC.Prim
import GHC.Exts hiding (fromString,toList)
import GHC.ST

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Data.Monoid
import Data.Foldable
import qualified Data.String as STR

import Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as S

import Data.Text.Internal (Text(..))
import Data.Text.Array (MArray(..),Array(..))
import Data.Text.Internal.Unsafe
import qualified Data.Text.Array as A
import qualified Data.Text.Internal.Unsafe.Char as UC (unsafeWrite)

{- Internal Mutable text data structure -}

-- | Buffer length in 'Word16's (ie 'bufferLength' 'Word16's per buffer)
bufferLength :: Int
bufferLength = 128

-- |Buffer used when evaluating builders
data Buffer s = Buffer {
  bufferArray :: !(MArray s), -- ^ contains 'Word16's
  bufferUsedLength :: !Int -- ^ number of 'Word16's in the buffer
}

-- |Like A.new, but doesn't check n for negativity
{-# INLINE unsafeNewBuffer #-}
unsafeNewBuffer :: ST s (Buffer s)
unsafeNewBuffer = ST $ \s -> case newByteArray# aryLen s of
  (# new_s, marr #) -> (# new_s, Buffer (MArray marr) 0 #)
  where !(I# aryLen) = bufferLength * 2

-- |Create a strict text out of a buffer.
{-# INLINE bufferToText #-}
bufferToText :: Buffer s -> ST s Text
bufferToText (Buffer (MArray b) len@(I# l)) = ST $ \s ->
  case shrinkMutableByteArray# b l s of
    s' -> case unsafeFreezeByteArray# b s' of
      (# s'',a #) -> (# s'', (Text (Array a) 0 len) #)

-- |Convert the current buffer to a 'Text' and append it to the
-- end of the sequence. Create a new current buffer.
-- TODO: Turn into a single ST action
{-# INLINE pushBuffer #-}
pushBuffer :: (Buffer s, Seq Text) -> ST s (Buffer s, Seq Text)
pushBuffer (b,xs) = do
  newH <- unsafeNewBuffer
  txt <- bufferToText b
  return (newH, xs |> txt)

-- |Append a char to the end of a buffered sequence
snocVec :: Char -> (Buffer s, Seq Text) -> ST s (Buffer s, Seq Text)
snocVec v tup@((Buffer a l),xs)
  | l < bufferLength = do
    n <- UC.unsafeWrite a l v -- writes a Char as a 'Word16's
    return (Buffer a (l+n),xs)
  | otherwise = pushBuffer tup >>= snocVec v

-- | Append a 'Text' to the end of a buffered text sequence
appendVec :: Text -> (Buffer s, Seq Text) -> ST s (Buffer s, Seq Text)
appendVec t@(Text ta to tl) tup@(mt@(Buffer a l),xs) = do
  performCopy copyLength
  if tl > copyLength  -- 'mt' can't accommodate tl bytes
    then pushBuffer tup >>= appendVec (Text ta (to+copyLength) (tl-copyLength))
    else return (Buffer a (l+copyLength), xs)
  where
    performCopy len = A.copyI a l ta to (l + len) -- copy 'len' bytes into 'mt'
    minLength = tl + l -- minimum length of buffer to hold mtext++text
    remaining = (bufferLength - l) -- bytes remaining in buffer
    copyLength = minTwo remaining tl -- bytes to copy
    minTwo a b | a < b = a | otherwise = b

{- Public API -}

-- |Wrapper around a function that applies changes
-- to a sequence of mutable buffers in 'ST'.
data Builder = Builder {
  runBuilder :: forall s. ((Buffer s, Seq Text) -> ST s (Buffer s, Seq Text))
             -> (Buffer s, Seq Text)
             -> ST s (Buffer s, Seq Text)
}

evalBuilder :: Builder -> ST s [Text]
evalBuilder (Builder f) = do
  (h,t) <- unsafeNewBuffer >>= f return . (,S.empty)
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
fromString s = fromText $ T.pack s-- Builder $ \f tup -> foldlM (flip snocVec) tup s >>= f

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

-- INTEGERS

decimal :: Integral a => a -> Builder
decimal v
  | v < 0 = singleton '-' <> decimal (abs v)
  | v == 0 = singleton '0'
  | otherwise = f mempty v
  where
    f :: (Integral a) => Builder -> a -> Builder
    f acc 0 = acc
    f acc v = let (q,r) = quotRem v 10
                  c = chr $ 48 + (fromIntegral r)
              in f ((singleton c) <> acc) q

-- TODO: two's compliment signed hex
-- |Render a *signed* hexadecimal number to a Builder
hexadecimal :: Integral a => a -> Builder
hexadecimal v
  | v < 0 = error "UNIMPLEMENTED: negative hexadecimal two's compliment representations."
  | v == 0 = singleton '0'
  | otherwise = f mempty v
  where
    f :: (Integral a) => Builder -> a -> Builder
    f acc 0 = acc
    f acc v = let (q,r) = quotRem v 16
                  c = hexChar r
              in f ((singleton c) <> acc) q
    hexChar v
      | v < 10 = chr $ 48 + (fromIntegral v)
      | otherwise = chr $ 65 + (fromIntegral v) - 10
      
realFloat :: RealFloat a => a -> Builder
realFloat v
  | v < 0 = singleton '-' <> realFloat (abs v)
  | v == 0.0 = fromString "0.0"
  | otherwise = let (sig,rad) = decodeFloat v
                in f mempty rad (abs sig)
  where
    -- TODO: add decimal point
    f :: Builder -> Int -> Integer -> Builder
    f acc _ 0 = acc
    f acc 0 v = let (q,r) = quotRem v 10 in f ((singleton $ decChar r) <> acc) 0 q
    f acc rdx v = let (q,r) = quotRem v 10 in f ((singleton $ decChar r) <> acc) (rdx-1) q
    decChar v = chr $ 48 + (fromIntegral v)