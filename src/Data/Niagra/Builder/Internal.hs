{-# LANGUAGE MagicHash, BangPatterns, UnboxedTuples, TupleSections #-}
module Data.Niagra.Builder.Internal
(
  Buffer(..),
  bufferLength,
  unsafeWriteChar,
  unsafeNewBuffer,
 --  newPinnedBuffer,
  bufferToText,
  pushBuffer,
  snocVec,
  appendVec
)
where

import GHC.Prim
import GHC.Exts
import GHC.ST
import GHC.Word (Word16(..))

import Data.Char
import Data.Bits ((.&.))
import Data.Text.Internal (Text(..))
import Data.Text.Array (Array(..))
import Data.Text.Internal.Unsafe.Shift (shiftR)
import Data.Sequence (Seq(..), (|>))

-- | Buffer length in 'Word16's (ie 'bufferLength' 'Word16's per buffer)
{-# INLINE bufferLength #-}
bufferLength :: Int
bufferLength = 128

-- |Buffer used when evaluating builders
data Buffer s = Buffer {
  bufferArray :: !(MutableByteArray# s), -- ^ contains 'Word16's
  bufferUsedLength :: !Int -- ^ number of 'Word16's in the buffer
}

copyBA :: MutableByteArray# s -- ^ Destination
       -> Int                 -- ^ Destination offset (in 'Word16's)
       -> ByteArray#          -- ^ Source
       -> Int                 -- ^ Source offset (in 'Word16's)
       -> Int                 -- ^ number of 'Word16's to copy
       -> ST s ()
copyBA dest (I# doff) src (I# soff) (I# n) = ST $ \s ->
  (# copyByteArray# src (uncheckedIShiftL# soff 1#)
                    dest (uncheckedIShiftL# doff 1#)
                    (uncheckedIShiftL# n 1#) s, () #)

writeWord16 :: MutableByteArray# s -> Int -> Word16 -> ST s ()
writeWord16 marr# (I# i#) (W16# w#) = ST $ \s -> (# (writeWord16Array# marr# i# w# s), () #)

shrinkBuffer :: Buffer s -> ST s ()
shrinkBuffer (Buffer a (I# l#)) = ST $ \s -> (# (shrinkMutableByteArray# a (uncheckedIShiftL# l# 1#) s), () #)

freezeBuffer :: Buffer s -> ST s Text
freezeBuffer (Buffer a l) = ST $ \s -> case unsafeFreezeByteArray# a s of
  (# s', ary #) -> (# s', Text (Array ary) 0 l #)

-- |Write a character into the array at the given offset. Returns
-- the number of 'Word16's written.
unsafeWriteChar :: MutableByteArray# s -> Int -> Char -> ST s Int
unsafeWriteChar a i c
  | n < 0x10000 = do
    writeWord16 a i $ fromIntegral n
    return 1
  | otherwise = do
    writeWord16 a i lo
    writeWord16 a (i+1) hi
    return 2
  where n = ord c
        m = n - 0x10000
        lo = fromIntegral $ (m `shiftR` 10) + 0xD800
        hi = fromIntegral $ (m .&. 0x3FF) + 0xDC00
        
-- |Create a new buffer.
unsafeNewBuffer :: ST s (Buffer s)
unsafeNewBuffer = ST $ \s -> case newByteArray# (uncheckedIShiftL# aryLen 1#) s of
  (# s', a #) -> (# s', Buffer a 0 #)
  where !(I# aryLen) = bufferLength * 2
  
-- |Create a strict text out of a buffer.
bufferToText :: Buffer s -> ST s Text
bufferToText b = shrinkBuffer b >> freezeBuffer b

-- |Convert the current buffer to a 'Text' and append it to the
-- end of the sequence. Create a new current buffer.
pushBuffer :: (Buffer s, Seq Text) -> ST s (Buffer s, Seq Text)
pushBuffer (b,xs) = do
  frzn <- bufferToText b
  (, xs |> frzn) <$> unsafeNewBuffer

-- |Append a char to the end of a buffered sequence
snocVec :: Char -> (Buffer s, Seq Text) -> ST s (Buffer s, Seq Text)
snocVec c tup@(Buffer a l, xs)
  | n < 0x10000 = do
    if l+1 > bufferLength
      then pushBuffer tup >>= snocVec c
      else do
        writeWord16 a l $ fromIntegral n
        return (Buffer a (l+1), xs)
  | otherwise = do
      if l+2 > bufferLength
        then pushBuffer tup >>= snocVec c
        else do
          writeWord16 a l lo
          writeWord16 a (l+1) hi
          return (Buffer a (l+2), xs)
  where n = ord c
        m = n - 0x10000
        lo = fromIntegral $ (m `shiftR` 10) + 0xD800
        hi = fromIntegral $ (m .&. 0x3FF) + 0xDC00

-- | Append a 'Text' to the end of a buffered text sequence
appendVec :: Text -> (Buffer s, Seq Text) -> ST s (Buffer s, Seq Text)
appendVec (Text ta@(Array tbuf) to tl) (Buffer a l, xs) = do
  copyBA a l tbuf to copyLength
  if tl > remaining -- 'mt' can't accommodate tl bytes
    then pushBuffer updatedTup >>= appendVec (Text ta (to+copyLength) (tl-copyLength))
    else return updatedTup
  where
    updatedTup = (Buffer a (l+copyLength), xs)
    remaining = bufferLength - l -- 'Word16's remaining in buffer
    copyLength = minTwo remaining tl -- 'Word16's to copy
    minTwo a b | a < b = a | otherwise = b