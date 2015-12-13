module Data.Niagra.Rule.FontFace
(
  FontFace(..)
)
where
  
import Data.Niagra.Buildable
import Data.ByteString.Builder

data FontFace = FontFace

instance Buildable FontFace where
  build _ = string8 "@font-face"