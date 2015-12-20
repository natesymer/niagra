{-# LANGUAGE OverloadedStrings #-}
module Data.Niagra.At.FontFace
(
  fontFace
)
where

import Data.Niagra.Monad
import Data.Niagra.DSL
  
-- |A @font-face
fontFace :: (Monad m) => NiagraT m () -- ^ content of the @font-face
                      -> NiagraT m ()
fontFace = block "@font-face"