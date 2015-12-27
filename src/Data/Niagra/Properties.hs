{-# LANGUAGE OverloadedStrings #-}
module Data.Niagra.Properties
(
  -- * Combinators
  url,
  -- * Modules
  module Data.Niagra.Properties.Background,
  module Data.Niagra.Properties.Border,
  module Data.Niagra.Properties.Box,
  module Data.Niagra.Properties.Color,
  module Data.Niagra.Properties.Font,
  module Data.Niagra.Properties.Misc,
  module Data.Niagra.Properties.Text,
  module Data.Niagra.Properties.Units
)
where
  
import Data.Niagra.Properties.Background
import Data.Niagra.Properties.Box
import Data.Niagra.Properties.Border
import Data.Niagra.Properties.Color
import Data.Niagra.Properties.Font
import Data.Niagra.Properties.Misc
import Data.Niagra.Properties.Text
import Data.Niagra.Properties.Units

import Data.Monoid
import Data.Niagra.Builder

url :: Builder -> Builder
url v = "url(\"" <> v <> "\")"