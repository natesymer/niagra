{-# LANGUAGE OverloadedStrings #-}

module Data.Niagra.Properties.Color
(
  -- * Declaration Combinators
  color,
  opacity,
  -- * Value Combinators
  hex,
  rgb,
  rgba
)

where
  
import Data.Niagra.Monad
import Data.Niagra.Builder
import Data.Niagra.Builder.Numbers
import Data.Monoid
  
-- TODO: HSL et al

color :: (Monad m) => Builder -> NiagraT m ()
color = declaration "color"

opacity :: (Monad m) => Double -> NiagraT m ()
opacity = declaration "opacity" . realFloat

hex :: Integer -> Builder
hex h = singleton '#' <> hexadecimal h

rgb :: Integer -> Integer -> Integer -> Builder
rgb r g b = "rgb(" <> decimal r
                   <> singleton ','
                   <> decimal g
                   <> singleton ','
                   <> decimal b
                   <> singleton ')'

rgba :: Integer -> Integer -> Integer -> Double -> Builder
rgba r g b a = "rgba(" <> decimal r
                       <> singleton ','
                       <> decimal g
                       <> singleton ','
                       <> decimal b
                       <> singleton ','
                       <> realFloat a
                       <> singleton ')'