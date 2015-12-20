{-# LANGUAGE OverloadedStrings #-}
module Data.Niagra.At.Media
(
  media
)
where

import Data.Niagra.Monad
import Data.Niagra.Block
import Data.Niagra.Selector

import Data.Text.Lazy (Text)

-- |A @media query.
media :: (Monad m) => Text
                   -> NiagraT (NiagraT m) () -- ^ content of the @media query
                   -> NiagraT m ()
media str act = execNiagraT Null act >>= addBlock . f
  where f b = BuilderBlock sel $ mconcat $ map buildBlock b
        sel = Raw $ mappend "@media " str