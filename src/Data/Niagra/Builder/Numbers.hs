module Data.Niagra.Builder.Numbers
(
  decimal,
  hexadecimal,
  realFloat
)
where

import Data.Niagra.Builder

import Data.Text.Internal.Fusion as TF
import Data.Text.Internal.Fusion.Size

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
  | v < 0     = nf v
  | otherwise =  f v
  where
    nf v = fromText $ TF.reverse $ Stream next (v,True) unknownSize
      where next (0,_) = Done
            next (v,True) = let (q,r) = quotRem v 16
                                r' = 15 - (abs r)
                            in Yield (hexChar (if r' == 15 then 0 else (r' + 1))) (q,r' == 15)
            next (v,False) = let (q,r) = quotRem v 16
                                 r' = 15 - (abs r)
                             in Yield (hexChar r') (q,False)
    f v = fromText $ TF.reverse $ Stream next v unknownSize
      where next 0 = Done
            next v = let (q,r) = quotRem v 16
                     in Yield (hexChar r) q
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
      