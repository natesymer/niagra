module Data.Niagra.Builder.Numbers
(
  decimal,
  hexadecimal,
  realFloat
)
where

import Data.Niagra.Builder

import Data.Char
import Data.Monoid
import Numeric

decimal :: Integral a => a -> Builder
decimal v
  | v < 0 = singleton '-' <> decimal (abs v)
  | v < 10 = singleton $ chr $ 48 + (fromIntegral v) -- prevent costly calc for single-digit numbers. This is a significant optimization, especially when building CSS (eg 2px).
  | otherwise = f mempty v
  where
    f acc 0 = acc
    f acc v = let (q,r) = quotRem v 10
                  c = chr $ 48 + (fromIntegral r)
              in f (singleton c <> acc) q

-- TODO: two's compliment signed hex
-- |Render a *signed* hexadecimal number to a Builder
hexadecimal :: Integral a => a -> Builder
hexadecimal v
  | v < 0 = error "UNIMPLEMENTED: negative hexadecimal two's compliment representations."
  | v == 0 = singleton '0'
  | otherwise = f mempty v
  where
    f acc 0 = acc
    f acc v = let (q,r) = quotRem v 16
                  c = hexChar r
              in f (singleton c <> acc) q
    hexChar v
      | v < 10 = chr $ 48 + (fromIntegral v)
      | otherwise = chr $ 65 + (fromIntegral v) - 10

realFloat :: (RealFloat a) => a -> Builder
realFloat v
  | v < 0 = singleton '-' <> realFloat (abs v)
  | otherwise = let (a,b) = floatToDigits 10 v
                    digits = map (chr . (+) 48) a
                    (leftOfDec,rightOfDec) = splitAt b digits
                in fromString leftOfDec <> singleton '.' <> fromString rightOfDec
      