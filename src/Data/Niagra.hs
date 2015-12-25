{-|
Module      : Data.Niagra
Description : Root module of Niagra
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Provides a basic interface for defining CSS
and rendering those blocks into strings.

Niagra produces "minified" CSS.
-}

{-# LANGUAGE OverloadedStrings #-}

module Data.Niagra
(
  -- * Modules
  module Data.Niagra.DSL,
  module Data.Niagra.At,
  module Data.Niagra.Monad,
  module Data.Niagra.Block,
  module Data.Niagra.Selector,
  module Data.Niagra.Selector.Tags,
  module Data.Niagra.Selector.Combinators,
  module Data.Niagra.Properties,
  module Data.Niagra.Value
)
where

import Data.Niagra.DSL
import Data.Niagra.At
import Data.Niagra.Monad
import Data.Niagra.Block
import Data.Niagra.Selector
import Data.Niagra.Selector.Tags
import Data.Niagra.Selector.Combinators
import Data.Niagra.Properties
import Data.Niagra.Value

{-
TODO (in no particular order)

* wrappers around 'declaration'
* type selector parts better
-}

