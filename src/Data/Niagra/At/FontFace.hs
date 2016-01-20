{-# LANGUAGE OverloadedStrings #-}
module Data.Niagra.At.FontFace
(
  fontFace
)
where

import Data.Niagra.Selector
import Data.Niagra.Monad
  
-- |A @font-face
fontFace :: (Monad m) => NiagraT m () -- ^ content of the @font-face
                      -> NiagraT m ()
fontFace = rootScope FontFace