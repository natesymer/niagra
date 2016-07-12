{-|
Module      : Data.Niagra.Builder.Buffer
Description : 'Buffer' data structure for 'Builder'.
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : stable
Portability : POSIX

Provides a data structure for storing bytes
from 'Char's and strict 'Text's. Can easily be
frozen into a strict 'Text'.

-}

-- TODO unwrap bufferLength & bufferRemaining

{-# LANGUAGE MagicHash, BangPatterns, UnboxedTuples, TupleSections #-}
module Data.Niagra.Builder.Buffer
(
  Buffer,
  bufferLength,
  bufferRemaining,
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
import GHC.Word (Word16(..))

import Data.Text.Internal (Text(..))
import Data.Text.Array (Array(..))

import Control.Monad.Primitive

-- |Buffer used when evaluating builders
data Buffer s = Buffer
                  !(MutableByteArray# s) -- ^ contains 'Word16's
                  !Int# -- ^ number of 'Word16's in the buffer
                  !Int# -- ^ remaining number of 'Word16's the buffer can accommodate

-- |Get the length of a buffer.
bufferLength :: Buffer s -> Int
bufferLength (Buffer _ l _) = I# l

-- |Get the remaining space in a buffer.
bufferRemaining :: Buffer s -> Int
bufferRemaining (Buffer _ _ r) = I# r

{- Buffer Operations -}

-- |Shrink a buffer to its length.
shrinkBuffer :: PrimMonad m
             => Buffer (PrimState m)
             -> m (Buffer (PrimState m))
shrinkBuffer b@(Buffer _ _ 0#) = return b
shrinkBuffer (Buffer a l _) = primitive $ \s -> case resizeMutableByteArray# a (l *# 2#) s of
  (# s', a' #) -> (# s', Buffer a' l 0# #)

-- |Freeze @buffer@ into a strict 'Text'.
freezeBuffer :: PrimMonad m
             => Buffer (PrimState m) -- ^ @buffer@
             -> m Text
freezeBuffer (Buffer a l _) = primitive $ \s -> case unsafeFreezeByteArray# a s of
  (# s', ary #) -> (# s', Text (Array ary) 0 (I# l) #)

-- |Create a new *unpinned* buffer.
newBuffer :: PrimMonad m
          => Int -- ^ length of buffer in 'Word16's
          -> m (Buffer (PrimState m))
newBuffer (I# len) = primitive $ \s -> case newByteArray# (len *# 2#) s of
  (# s', a #) -> (# s', Buffer a 0# len #)
  
-- |Append a strict 'Text' to a 'Buffer'.
bufferAppendText :: PrimMonad m
                 => Text -- ^ text to append
                 -> Int -- ^ length to copy
                 -> Buffer (PrimState m) -- ^ buffer to copy into
                 -> m (Buffer (PrimState m)) -- ^ the buffer after the copy
bufferAppendText (Text (Array tbuf) (I# to) _)
                 (I# cl)
                 (Buffer a l r) = primitive $ \s ->
  (#
    copyByteArray# tbuf (to *# 2#)
                   a (l *# 2#)
                   (cl *# 2#) s,
    Buffer a (l +# cl) (r -# cl)
  #)
  
-- |Append a 'Word16' to a 'Buffer'.
bufferAppendWord16 :: PrimMonad m
                   => Word16 -- ^ 'Word16' to append
                   -> Buffer (PrimState m) -- ^ @buffer@
                   -> m (Buffer (PrimState m)) -- ^ @buffer@ after 
bufferAppendWord16 (W16# w) (Buffer a l r) = primitive $ \s ->
  (# writeWord16Array# a l w s, Buffer a (l +# 1#) (r -# 1#) #)

{- Text & Char operations -}

-- |Convert a char into either a 'Word16' or a
-- pair of 'Word16's of UTF-16 encoded chars
charToWord16 :: Char -> Either Word16 (Word16,Word16)
charToWord16 (C# c) = case n <# 0x10000# of
  1# -> Left $ int2Word16 n
  _  -> case (n `andI#` 0x1ff800#) of
          0xd800# -> Left 0xfffd -- see comment above 'Data.Text.Internal.safe'
          _       -> Right (int2Word16 lo, int2Word16 hi)
  where !n = ord# c
        !m = n -# 0x10000#
        !lo = (m `uncheckedIShiftRA64#` 10#) +# 0xD800#
        !hi = (m `andI#` 0x3FF#) +# 0xDC00#
        int2Word16 i = W16# (narrow16Word# (int2Word# i)) -- TODO: make sure this converts to a Word16 and not just the platform word size

-- |Remove 'Word16's off the front of a strict 'text'
-- without having to copy it.
{-# INLINE offsetText #-}
offsetText :: Text -> Int -> Text
offsetText (Text a o l) off = Text a (o+off) (l-off)
