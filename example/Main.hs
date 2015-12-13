{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
  
import Data.Niagra
import Data.Monoid
import Data.ByteString.Builder (Builder,toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = BL.putStrLn $ toLazyByteString example

example :: Builder
example = cssBuilder' $ do
  a # "title" ? do
    "background-color" .= "red"
    "color"            .= "green"
    
  a >| (h2 ! "myclass") <> a # "title" ? do
    "background-color" .= "red"
    "color"            .= "green"
    
  ident "this" ? do
    "position" .= "relative"
    
  h2 <||> ("foo" <^=> "bar") ? do
    "background-color" .= "red"
    "color"            .= "green"
    
  a <:> (PseudoClass "visited" Nothing) ? do
    "color" .= "red"
    
  input ? do
    "background-color" .= "red"
    --
    -- "type" <=> "text" ? do
    --   -- style for text boxes
    
  fontFace $ do
    "src" .= "url(/assets/fonts/oxygen/Oxygen-Bold.woff2)"