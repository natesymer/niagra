{-|
Module      : Data.Niagra.Builder.Buffer
Description : 'Buffer' data structure for 'Builder'.
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Provides a data structure for storing bytes
from 'Char's and strict 'Text's. Can easily be
frozen into a strict 'Text'.

-}

{-# LANGUAGE MagicHash, BangPatterns, UnboxedTuples, TupleSections #-}
module Data.Niagra.Builder.Buffer
(
  Buffer(..),
  -- * Generic Low-Level Operations
  copyWord16Array,
  -- * 'Buffer' Operations
  shrinkBuffer,
  freezeBuffer,
  newBuffer,
  bufferAppendText,
  bufferAppendWord16,
  -- * 'Text' & 'Char' Operations
  charToWord16,
  offsetText
)
where

import GHC.Prim
import GHC.Exts
import GHC.ST
import GHC.Word (Word16(..))

import Data.Char
import Data.Text.Internal (Text(..),safe)
import Data.Text.Array (Array(..))

-- |Buffer used when evaluating builders
data Buffer s = Buffer {
  bufferArray :: !(MutableByteArray# s), -- ^ contains 'Word16's
  bufferLength :: !Int, -- ^ number of 'Word16's in the buffer
  bufferRemaining :: !Int -- ^ remaining number of 'Word16's the buffer can accommodate
}

{- Generic Low-Level Operations -}

-- |Essentially a memcpy() modified to work in terms of 'Word16's.
copyWord16Array :: MutableByteArray# s -- ^ Destination
                -> Int                 -- ^ Destination offset (in 'Word16's)
                -> ByteArray#          -- ^ Source
                -> Int                 -- ^ Source offset (in 'Word16's)
                -> Int                 -- ^ number of 'Word16's to copy
                -> ST s ()
copyWord16Array dest (I# doff) src (I# soff) (I# n) = ST $ \s ->
  (# 
    copyByteArray# src (soff *# 2#)
                   dest (doff *# 2#)
                   (n *# 2#)
                   s,
    ()
  #)

{- Buffer Operations -}

-- |Shrink a buffer to its length.
shrinkBuffer :: Buffer s -> ST s (Buffer s)
shrinkBuffer b@(Buffer _ _ 0) = return b
shrinkBuffer (Buffer a (I# l#) _) = ST $ \s ->
  (#
  shrinkMutableByteArray# a (l# *# 2#) s,
  Buffer a (I# l#) (I# l#)
  #)

-- |Freeze a buffer into a strict 'Text'.
freezeBuffer :: Buffer s -> ST s Text
freezeBuffer (Buffer a l _) = ST $ \s -> case unsafeFreezeByteArray# a s of
  (# s', ary #) -> (# s', Text (Array ary) 0 l #)

-- |Create a new *unpinned* buffer inside the GC.
newBuffer :: Int -> ST s (Buffer s)
newBuffer i@(I# aryLen) = ST $ \s -> case newByteArray# (aryLen *# 2#) s of
  (# s', a #) -> (# s', Buffer a 0 i #)
  
-- |Append a strict 'Text' to a 'Buffer'.
bufferAppendText :: Text -> Int -> Buffer s -> ST s (Buffer s)
bufferAppendText (Text (Array tbuf) to tl) copyLen (Buffer a l remain) = do
  copyWord16Array a l tbuf to copyLen
  return $ Buffer a (l+copyLen) (remain-copyLen)
  
-- |Append a 'Word16' to a 'Buffer'.
bufferAppendWord16 :: Buffer s -> Word16 -> ST s (Buffer s)
bufferAppendWord16 (Buffer a l@(I# l#) r) (W16# w) = ST $ \s ->
  (#
    writeWord16Array# a l# w s,
    Buffer a (l+1) (r-1)
  #)

{- Text & Char operations -}

-- |Convert a char into either a 'Word16' or a pair of 'Word16's
{-# INLINE charToWord16 #-}
charToWord16 :: Char -> Either Word16 (Word16,Word16)
charToWord16 c
  | o < 0x10000 = Left $ intToWord16 n
  | otherwise = Right $ (intToWord16 lo, intToWord16 hi)
  where !o@(I# n) = ord $ safe c
        m = n -# 0x10000#
        lo = (m `uncheckedIShiftRA64#` 10#) +# 0xD800#
        hi = (m `andI#` 0x3FF#) +# 0xDC00#
        intToWord16 i = W16# (int2Word# i)

{-# INLINE offsetText #-}
-- |Remove 'Word16's off the front of a strict 'text'
-- without having to copy it.
offsetText :: Text -> Int -> Text
offsetText (Text a o l) off = Text a (o+off) (l-off)