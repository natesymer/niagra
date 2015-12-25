{-# LANGUAGE OverloadedStrings #-}

module Data.Niagra.Properties.Misc
(
  content,
  cursor,
  appearance
)
where
  
import Data.Niagra.Monad
import Data.Niagra.DSL

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