{-# LANGUAGE RankNTypes, MagicHash #-}
module Data.Niagra.Builder
(
  Builder(..),
  singleton,
  fromString,
  fromText,
  toText,
  decimal,
  hexadecimal,
  realFloat
)
where

import Data.Text (Text)
import Data.Monoid
import qualified Data.String as STR
import Data.Char

import qualified Data.Text.Internal.Unsafe.Char as UC (unsafeWrite)
import Data.Text.Array (MArray(..), Array(..))
import qualified Data.Text.Array as A
import Data.Text.Internal (Text(..),text)
import GHC.Prim
import GHC.Exts (Int(..))
import Data.Foldable

import Control.Monad.ST

{- Internal Mutable text data structure -}

-- mutable version of Text
data MText s = MText {
  mtextArray :: MArray s,
  mtextOffset :: !Int,
  mtextLength :: !Int
}

lengthMArray :: forall s. MArray s -> Int
lengthMArray (MArray a) = I# (sizeofMutableByteArray# a)

lengthArray :: Array -> Int
lengthArray (Array a) = I# (sizeofByteArray# a)

newText :: forall s. Int -> ST s (MText s) -- forall s. Int -> ST s (MText s)
newText s = A.new s >>= \mt -> return $ MText mt 0 0
  
snocVec :: Char -> MText s -> ST s (MText s)
snocVec v (MText a o l)
  | (l+1)*2 > (lengthMArray a) = do
    mt <- A.new ((lengthMArray a)*2)
    A.copyM mt 0 a 0 (lengthMArray a)
    snocVec v $ MText mt o l
  | otherwise = do
    UC.unsafeWrite a l v -- writes a Char as a Word16
    return $ MText a o (l+1)
    
appendVec :: Text -> MText s -> ST s (MText s)
appendVec t@(Text ia io il) (MText a o l)
  | ((il + l + o) * 2) > (lengthMArray a) = do
    n <- A.new $ ((lengthMArray a) * 2) + (il * 2)
    A.copyM n 0 a (o*2) (l*2)
    appendVec t $ MText n 0 $ l + il
  | otherwise = do
    A.copyI a (o+l) ia io (o+l+il) -- copy t into mutable text
    return $ MText a o (l+il)

{- Public API -}

data Builder = Builder {
  runBuilder :: forall s. (ST s (MText s) -> ST s (MText s)) -> ST s (MText s) -> ST s (MText s)
}

instance Monoid Builder where
  mempty  = empty
  mappend = appendBuilder
  mconcat = foldl' mappend mempty
  
instance STR.IsString Builder where
  fromString = fromString

empty :: Builder
empty = Builder $ \f v -> f v
    
singleton :: Char -> Builder
singleton c = Builder $ \f v -> v >>= f . snocVec c

fromString :: String -> Builder
fromString [] = empty
fromString xs = Builder $ \f v -> v >>= \v' -> f $ foldlM (flip snocVec) v' xs

fromText :: Text -> Builder
fromText t = Builder $ \f v -> v >>= f . appendVec t

appendBuilder :: Builder -> Builder -> Builder
appendBuilder (Builder a) (Builder b) = Builder $ a . b
  
toText :: Builder -> Text
toText (Builder f) = runST $ do
  (MText m o l) <- f id (newText 128)
  a <- A.unsafeFreeze m
  return $ text a o l
  

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