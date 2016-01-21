module Data.Niagra.Builder.Numbers
(
  decimal,
  hexadecimal,
  realFloat
)
where

import Data.Niagra.Builder

import Data.List
import Data.Char
import Data.Monoid
import Numeric

decimal :: Integral a => a -> Builder
decimal v
  | v < 0 = singleton '-' <> decimal (abs v)
  | v < 10 = decChar v -- prevent costly calc for single-digit numbers. This is a significant optimization, especially when building CSS (eg 2px).
  | otherwise = f mempty v
  where
    decChar c = singleton $ chr $ 48 + (fromIntegral c)
    f acc 0 = acc
    f acc v = let (q,r) = quotRem v 10
                  c = decChar r
              in f (c <> acc) q

-- |Render a *signed* hexadecimal number to a Builder
hexadecimal :: (Integral a) => a -> Builder
hexadecimal 0 = singleton '0'
hexadecimal v
  | v < 0 = nf v
  | otherwise = f mempty v
  where
    nf v = let u 0 = Nothing
               u b = let (q,r) = quotRem b 16
                     in Just (15-(abs r),q)
               digits = reverse $ addOne $ unfoldr u v
               addOne [] = []
               addOne (15:xs) = 0:addOne xs
               addOne (x:xs) = x+1:xs
           in mconcat $ map (singleton . hexChar) digits
    f acc 0 = acc
    f acc v = let (q,r) = quotRem v 16
                  c = hexChar r
              in f (singleton c <> acc) q
    hexChar v
      | v < 10 = chr $ 48 + (fromIntegral v)
      | otherwise = chr $ 55 + (fromIntegral v)

realFloat :: (RealFloat a) => a -> Builder
realFloat v
  | v < 0 = singleton '-' <> realFloat (abs v)
  | otherwise = let (a,b) = floatToDigits 10 v
                    digits = map (chr . (+) 48) a
                    (leftOfDec,rightOfDec) = splitAt b digits
                in fromString leftOfDec <> singleton '.' <> fromString rightOfDec
      