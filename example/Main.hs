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

  a <:> "visited" ? do
    "color" .= "red"

  input ? do
    "background-color" .= "red"
    
    a ? do
      "background-color" .= "green"
    
    -- subselector a (Equality "type" "text")] $ do
    --
    -- subselector h2 $ return [Right (PseudoClass "visited" Nothing)]
    --
    -- "type" <=> "text" ? do
    --   -- style for text boxes
  (Raw "@font-face") ? do
    "src" .= "url(/assets/fonts/oxygen/Oxygen-Bold.woff2)"
  --
  -- Media "screen" ? do
  --   a ? do
  --     "background-color" .= "green"