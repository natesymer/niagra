{-# LANGUAGE OverloadedStrings #-}
module Data.Niagra.At.Media
(
  media
)
where

import Data.Niagra.Monad
import Data.Niagra.Block
import Data.Niagra.Selector
import Data.Niagra.DSL

import Data.Text (Text)

-- |A @media query.
media :: (Monad m) => Text
                   -> NiagraT (NiagraT m) () -- ^ content of the @media query
                   -> NiagraT m ()
media str act = cssBuilder act >>= addBlock . BuilderBlock sel 
  where sel = Raw $ mappend "@media " str