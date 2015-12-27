{-# LANGUAGE OverloadedStrings #-}

module Data.Niagra.Properties.Background
(
  -- * Declaration Combinators
  background,
  backgroundColor,
  backgroundImage,
  backgroundRepeat,
  backgroundSize,
  boxShadow
)
where
  
import Data.Niagra.Monad
import Data.Niagra.DSL
import Data.Niagra.Value

import Data.Niagra.Builder
  
background :: (Monad m, Value v) => v -> NiagraT m ()
background = declaration "background" . build
  
backgroundColor :: (Monad m) => Builder -> NiagraT m ()
backgroundColor = declaration "background-color"

backgroundImage :: (Monad m) => Builder -> NiagraT m ()
backgroundImage = declaration "background-image"

backgroundRepeat :: (Monad m) => Builder -> NiagraT m ()
backgroundRepeat = declaration "background-repeat"

backgroundSize :: (Monad m) => Builder -> NiagraT m ()
backgroundSize = declaration "background-size"

boxShadow :: (Monad m, Value v) => v -> NiagraT m ()
boxShadow = declaration "box-shadow" . build