{-# LANGUAGE OverloadedStrings #-}

module Data.Niagra.Properties.Box
(
  -- * Absolute Positioning
  bottom,
  top,
  right,
  left,
  width,
  height,
  -- * Margin
  margin,
  marginTop,
  marginBottom,
  marginLeft,
  marginRight,
  -- * Padding
  padding,
  paddingTop,
  paddingBottom,
  paddingLeft,
  paddingRight,
  -- * Sizing
  maxHeight,
  maxWidth,
  minHeight,
  minWidth,
  -- * Miscellaneous
  zIndex,
  verticalAlign,
  display,
  float,
  position
)
where

import Data.Niagra.Monad
import Data.Niagra.Value
import Data.Niagra.Builder
import Data.Niagra.Builder.Numbers

bottom :: (Monad m) => Builder -> NiagraT m ()
bottom = declaration "bottom"

top :: (Monad m) => Builder -> NiagraT m ()
top = declaration "top"

right :: (Monad m) => Builder -> NiagraT m ()
right = declaration "right"

left :: (Monad m) => Builder -> NiagraT m ()
left = declaration "left"

width :: (Monad m) => Builder -> NiagraT m ()
width = declaration "width"

height :: (Monad m) => Builder -> NiagraT m ()
height = declaration "height"

margin :: (Monad m, Value v) => v -> NiagraT m ()
margin = declaration "margin" . build

marginTop :: (Monad m) => Builder -> NiagraT m ()
marginTop = declaration "margin-top"

marginBottom :: (Monad m) => Builder -> NiagraT m ()
marginBottom = declaration "margin-bottom"

marginLeft :: (Monad m) => Builder -> NiagraT m ()
marginLeft = declaration "margin-left"

marginRight :: (Monad m) => Builder -> NiagraT m ()
marginRight = declaration "margin-right"

padding :: (Monad m, Value v) => v -> NiagraT m ()
padding = declaration "padding" . build

paddingTop :: (Monad m) => Builder -> NiagraT m ()
paddingTop = declaration "padding-top"

paddingBottom :: (Monad m) => Builder -> NiagraT m ()
paddingBottom = declaration "padding-bottom"

paddingLeft :: (Monad m) => Builder -> NiagraT m ()
paddingLeft = declaration "padding-left"

paddingRight :: (Monad m) => Builder -> NiagraT m ()
paddingRight = declaration "padding-right"

maxHeight :: (Monad m) => Builder -> NiagraT m ()
maxHeight = declaration "max-height"

maxWidth :: (Monad m) => Builder -> NiagraT m ()
maxWidth = declaration "max-width"

minHeight :: (Monad m) => Builder -> NiagraT m ()
minHeight = declaration "min-height"

minWidth :: (Monad m) => Builder -> NiagraT m ()
minWidth = declaration "min-width"

zIndex :: (Monad m) => Integer -> NiagraT m ()
zIndex = declaration "z-index" . decimal

verticalAlign :: (Monad m) => Builder -> NiagraT m ()
verticalAlign = declaration "vertical-align"

display :: (Monad m) => Builder -> NiagraT m ()
display = declaration "display"

float :: (Monad m) => Builder -> NiagraT m ()
float = declaration "float"

position :: (Monad m) => Builder -> NiagraT m ()
position = declaration "position"

{-
TODO

clear	Specifies which sides of an element where other floating elements are not allowed	1
clip	Clips an absolutely positioned element	2
overflow Specifies what happens if content overflows an element's box	2
overflow-x	Specifies whether or not to clip the left/right edges of the content, if it overflows the element's content area	3
overflow-y	Specifies whether or not to clip the top/bottom edges of the content, if it overflows the element's content area	3
visibility	Specifies whether or not an element is visible	2

-}

{-

TODO?

flexible box properties?
  
-}