{-# LANGUAGE OverloadedStrings #-}

module Data.Niagra.Properties.Misc
(
  content,
  cursor,
  appearance,
  outline,
  outlineColor,
  outlineOffset,
  outlineStyle,
  outlineWidth
)
where
  
import Data.Niagra.Monad
import Data.Niagra.DSL
import Data.Niagra.Value

import Data.Monoid
import Data.Text.Lazy.Builder (Builder,singleton)

content :: (Monad m) => Builder -> NiagraT m ()
content b = declaration "content" $ singleton '\'' <> b <> singleton '\''

appearance :: (Monad m) => Builder -> NiagraT m ()
appearance a = do
  declaration "appearance" a
  declaration "-moz-appearance" a
  declaration "-webkit-appearance" a
  
cursor :: (Monad m) => Builder -> NiagraT m ()
cursor = declaration "cursor"

outline :: (Monad m, Value v) => v -> NiagraT m ()
outline = declaration "outline" . build

outlineColor :: (Monad m) => Builder -> NiagraT m ()
outlineColor = declaration "outline-color"

outlineOffset :: (Monad m) => Builder -> NiagraT m ()
outlineOffset = declaration "outline-offset"

outlineStyle :: (Monad m) => Builder -> NiagraT m ()
outlineStyle = declaration "outline-style"

outlineWidth :: (Monad m) => Builder -> NiagraT m ()
outlineWidth = declaration "outline-width"