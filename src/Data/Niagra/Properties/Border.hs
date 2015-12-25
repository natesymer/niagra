{-# LANGUAGE OverloadedStrings #-}

module Data.Niagra.Properties.Border
(
  -- * Basic Combinators
  border,
  borderRight,
  borderLeft,
  borderTop,
  borderBottom,
  -- ** Color
  borderColor,
  borderRightColor,
  borderLeftColor,
  borderTopColor,
  borderBottomColor,
  -- ** Style
  borderStyle,
  borderRightStyle,
  borderLeftStyle,
  borderTopStyle,
  borderBottomStyle,
  -- ** Width
  borderWidth,
  borderRightWidth,
  borderLeftWidth,
  borderTopWidth,
  borderBottomWidth,
  -- ** Radius
  borderRadius,
  borderBottomLeftRadius,
  borderBottomRightRadius,
  borderTopLeftRadius,
  borderTopRightRadius
)
where
  
import Data.Niagra.Monad
import Data.Niagra.DSL
import Data.Niagra.Value

import Data.Text.Lazy.Builder

{-
TODO:
border-image	A shorthand property for setting all the border-image-* properties	3
border-image-outset	Specifies the amount by which the border image area extends beyond the border box	3
border-image-repeat	Specifies whether the border image should be repeated, rounded or stretched	3
border-image-slice	Specifies how to slice the border image	3
border-image-source	Specifies the path to the image to be used as a border	3
border-image-width	Specifies the widths of the image-border	3
-}

{-
box-decoration-break	Sets the behaviour of the background and border of an element at page-break, or, for in-line elements, at line-break.	3
box-shadow	Attaches one or more drop-shadows to the box	3
-}

{- Basic border combinators -}

border :: (Monad m, Value v) => v -> NiagraT m ()
border = declaration "border" . build

borderColor :: (Monad m, Value v) => v -> NiagraT m ()
borderColor = declaration "border-color" . build

borderStyle :: (Monad m, Value v) => v -> NiagraT m ()
borderStyle = declaration "border-style" . build

borderWidth :: (Monad m, Value v) => v -> NiagraT m ()
borderWidth = declaration "border-width" . build

-- Right

borderRight :: (Monad m, Value v) => v -> NiagraT m ()
borderRight = declaration "border-right" . build

borderRightColor :: (Monad m) => Builder -> NiagraT m ()
borderRightColor = declaration "border-right-color"

borderRightStyle :: (Monad m) => Builder -> NiagraT m ()
borderRightStyle = declaration "border-right-style"

borderRightWidth :: (Monad m) => Builder -> NiagraT m ()
borderRightWidth = declaration "border-right-width"

-- Left

borderLeft :: (Monad m, Value v) => v -> NiagraT m ()
borderLeft = declaration "border-left" . build

borderLeftColor :: (Monad m) => Builder -> NiagraT m ()
borderLeftColor = declaration "border-left-color"

borderLeftStyle :: (Monad m) => Builder -> NiagraT m ()
borderLeftStyle = declaration "border-left-style"

borderLeftWidth :: (Monad m) => Builder -> NiagraT m ()
borderLeftWidth = declaration "border-left-width"

-- Top

borderTop :: (Monad m, Value v) => v -> NiagraT m ()
borderTop = declaration "border-top" . build

borderTopColor :: (Monad m) => Builder -> NiagraT m ()
borderTopColor = declaration "border-top-color"

borderTopStyle :: (Monad m) => Builder -> NiagraT m ()
borderTopStyle = declaration "border-top-style"

borderTopWidth :: (Monad m) => Builder -> NiagraT m ()
borderTopWidth = declaration "border-top-width"

-- Bottom

borderBottom :: (Monad m, Value v) => v -> NiagraT m ()
borderBottom = declaration "border-bottom" . build

borderBottomColor :: (Monad m) => Builder -> NiagraT m ()
borderBottomColor = declaration "border-bottom-color"

borderBottomStyle :: (Monad m) => Builder -> NiagraT m ()
borderBottomStyle = declaration "border-bottom-style"

borderBottomWidth :: (Monad m) => Builder -> NiagraT m ()
borderBottomWidth = declaration "border-bottom-width"

-- Radius

borderRadius :: (Monad m, Value v) => v -> NiagraT m ()
borderRadius = declaration "border-radius" . build

borderBottomLeftRadius :: (Monad m) => Builder -> NiagraT m ()
borderBottomLeftRadius = declaration "border-bottom-left-radius"

borderBottomRightRadius :: (Monad m) => Builder -> NiagraT m ()
borderBottomRightRadius = declaration "border-bottom-right-radius"

borderTopLeftRadius :: (Monad m) => Builder -> NiagraT m ()
borderTopLeftRadius = declaration "border-top-left-radius"

borderTopRightRadius :: (Monad m) => Builder -> NiagraT m ()
borderTopRightRadius = declaration "border-top-right-radius"