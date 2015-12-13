module Data.Niagra.Buildable where

import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BL

class Buildable a where
  build :: a -> Builder
  render :: a -> String
  render = BL.unpack . toLazyByteString . build