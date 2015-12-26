{-# LANGUAGE OverloadedStrings, RankNTypes, GADTs #-}

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
  
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.RealFloat
import Data.Text.Lazy.Builder.Int
import Data.Monoid
import Prelude hiding (rem)

data Measure where
  Measure :: Text -> Integer -> Measure
  MeasureDec :: Text -> Double -> Measure
  MeasureAuto :: Measure
  MeasureCalculated :: Operation -> Measure -> Measure -> Measure
 
instance Value Measure where
  build (Measure _ 0) = decimal 0 -- exclude units for 0
  build (Measure u s) = decimal s <> fromLazyText u
  build (MeasureDec _ 0.0) = realFloat 0.0 -- exclude units for 0
  build (MeasureDec u s) = realFloat s <> fromLazyText u
  build MeasureAuto = "auto"
   
data Operation = OpAdd | OpSub | OpDiv | OpMul

instance Value Operation where
  build OpAdd = singleton '+'
  build OpSub = singleton '-'
  build OpDiv = singleton '/'
  build OpMul = singleton '*'
  
-- TODO basic calculations to avoid MeasureCalculated
calc :: Operation -> Builder -> Builder -> Builder
calc op a b = mconcat ["calc(", a, singleton ' ', build op, singleton ' ', b, singleton ')']

{- ABSOLUTE UNITS -}

px,cm,mm,inch,pt,pc,unitless :: Integer -> Builder
px',cm',mm',inch',pt',pc',unitless' :: Double -> Builder
       
-- |Measured in pixels.
px  = build . Measure "px"
px' = build . MeasureDec "px"

-- |Measured in centimeters.
cm  = build . Measure "cm"
cm' = build . MeasureDec "cm"

-- |Measured in millimeters.
mm  = build . Measure "mm"
mm' = build . MeasureDec "mm"

-- |Measured in inches.
inch  = build . Measure "in"
inch' = build . MeasureDec "in"

-- |Measured in points.
pt  = build . Measure "pt"
pt' = build . MeasureDec "pt"

-- |Measured in picas.
pc  = build . Measure "pc"
pc' = build . MeasureDec "pc"

-- |Measured without units.
unitless  = build . Measure ""
unitless' = build . MeasureDec ""

{- RELATIVE UNITS -}

em,ex,ch,rem,vw,vh,vmin,vmax,perc :: Integer -> Builder
em',ex',ch',rem',vw',vh',vmin',vmax',perc' :: Double -> Builder

-- |Measured relative to element's @font-size@.
em  = build . Measure "em"
em' = build . MeasureDec "em"

-- |Measured relative to the x-height of the current font.
ex  = build . Measure "ex"
ex' = build . MeasureDec "ex"

-- |Measured relative to width of the "0".
ch  = build . Measure "ch"
ch' = build . MeasureDec "ch"

-- |Measured relative to @font-size@ of the root element.
rem  = build . Measure "rem"
rem' = build . MeasureDec "rem"

-- |Measured relative to 1% of the width of the viewport.
vw  = build . Measure "vw"
vw' = build . MeasureDec "vw"

-- |Measured relative to 1% of the height of the viewport.
vh  = build . Measure "vh"
vh' = build . MeasureDec "vh"

-- |Measured relative to 1% of viewport's smaller dimension.
vmin  = build . Measure "vmin"
vmin' = build . MeasureDec "vmin"

-- |Measured relative to 1% of viewport's larger dimension.
vmax  = build . Measure "vmax"
vmax' = build . MeasureDec "vmax"

-- |Measured by percent.
perc  = build . Measure "%"
perc' = build . MeasureDec "%"

-- |CSS @auto@.
auto :: Builder
auto = build $ MeasureAuto
