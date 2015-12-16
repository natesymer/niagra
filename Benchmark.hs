{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
  
import Criterion.Main
import Data.Niagra
import Data.Monoid
import Data.Text.Lazy.Builder
import Control.Monad.IO.Class

main :: IO ()
main = defaultMain [bgroup "niagra" [bench "basic" $ whnf toLazyText basic]]

basic :: Builder
basic = cssBuilder' $ do
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

    "type" <=> "text" ? do
      "border" .= "none"
      
  fontFace ? do
    "src" .= "url(/assets/fonts/oxygen/Oxygen-Bold.woff2)"
  
  media "screen" $ do
    body ? do
      "background-color" .= "lightgreen"