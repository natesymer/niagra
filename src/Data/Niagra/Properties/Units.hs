{-# LANGUAGE OverloadedStrings #-}

module Data.Niagra.Properties.Units
(
  -- * Types
  Operation(..),
  -- * Calculations
  calc,
  -- * Units
  -- ** Absolute Units
  px,
  px',
  cm,
  cm',
  mm,
  mm',
  inch,
  inch',
  pt,
  pt',
  pc,
  pc',
  unitless,
  unitless',
  -- ** Relative Units
  em,
  em',
  ex,
  ex',
  ch,
  ch',
  rem,
  rem',
  vw,
  vw',
  vh,
  vh',
  vmin,
  vmin',
  vmax,
  vmax',
  perc,
  perc',
  auto
)
where
  
import Data.Niagra.Value
import Data.Niagra.Builder
import Data.Niagra.Builder.Numbers
  
import Data.Text (Text)
import Data.Monoid
import Prelude hiding (rem)
   
data Operation = OpAdd | OpSub | OpDiv | OpMul

instance Value Operation where
  build OpAdd = singleton '+'
  build OpSub = singleton '-'
  build OpDiv = singleton '/'
  build OpMul = singleton '*'
  
-- TODO basic calculations to avoid MeasureCalculated
calc :: Operation -> Builder -> Builder -> Builder
calc op a b = mconcat ["calc(", a, singleton ' ', build op, singleton ' ', b, singleton ')']

wholeMeas :: Text -> Integer -> Builder
wholeMeas _ 0 = decimal 0
wholeMeas t v = decimal v <> fromText t

floatMeas :: Text -> Double -> Builder
floatMeas _ 0.0 = realFloat 0.0
floatMeas t v = realFloat v <> fromText t

{- ABSOLUTE UNITS -}

px,cm,mm,inch,pt,pc,unitless :: Integer -> Builder
px',cm',mm',inch',pt',pc',unitless' :: Double -> Builder
       
-- |Measured in pixels.
px = wholeMeas "px"
px' = floatMeas "px"

-- |Measured in centimeters.
cm  = wholeMeas "cm"
cm' = floatMeas "cm"

-- |Measured in millimeters.
mm  = wholeMeas "mm"
mm' = floatMeas "mm"

-- |Measured in inches.
inch  = wholeMeas "in"
inch' = floatMeas "in"

-- |Measured in points.
pt  = wholeMeas "pt"
pt' = floatMeas "pt"

-- |Measured in picas.
pc  = wholeMeas "pc"
pc' = floatMeas "pc"

-- |Measured without units.
unitless  = decimal
unitless' = realFloat

{- RELATIVE UNITS -}

em,ex,ch,rem,vw,vh,vmin,vmax,perc :: Integer -> Builder
em',ex',ch',rem',vw',vh',vmin',vmax',perc' :: Double -> Builder

-- |Measured relative to element's @font-size@.
em  = wholeMeas "em"
em' = floatMeas "em"

-- |Measured relative to the x-height of the current font.
ex  = wholeMeas "ex"
ex' = floatMeas "ex"

-- |Measured relative to width of the "0".
ch  = wholeMeas "ch"
ch' = floatMeas "ch"

-- |Measured relative to @font-size@ of the root element.
rem  = wholeMeas "rem"
rem' = floatMeas "rem"

-- |Measured relative to 1% of the width of the viewport.
vw  = wholeMeas "vw"
vw' = floatMeas "vw"

-- |Measured relative to 1% of the height of the viewport.
vh  = wholeMeas "vh"
vh' = floatMeas "vh"

-- |Measured relative to 1% of viewport's smaller dimension.
vmin  = wholeMeas "vmin"
vmin' = floatMeas "vmin"

-- |Measured relative to 1% of viewport's larger dimension.
vmax  = wholeMeas "vmax"
vmax' = floatMeas "vmax"

-- |Measured by percent.
perc  = wholeMeas "%"
perc' = floatMeas "%"

-- |CSS @auto@.
auto :: Builder
auto = fromText "auto"
