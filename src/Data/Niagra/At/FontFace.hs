{-# LANGUAGE OverloadedStrings #-}
module Data.Niagra.At.FontFace
(
  fontFace
)
where

import Data.Niagra.Monad
import Data.Niagra.Block
  
-- |A @font-face
fontFace :: (Monad m) => NiagraT (NiagraT m) () -- ^ content of the @font-face
                      -> NiagraT m ()
fontFace act = niagraDeclarations act >>= writeBlocks . f
  where f b = [DeclarationBlock "@font-face" b]