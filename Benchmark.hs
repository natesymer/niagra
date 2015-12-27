{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
  
import Criterion.Main
import Data.Niagra
import Data.Monoid
import Prelude hiding (span)

main :: IO ()
main = defaultMain [bgroup "niagra" [bench "basic" $ whnf toText basic,
                                     bench "wordlist" $ whnf toText wordlist]]

basic :: Builder
basic = cssBuilder' $ do
  a # "title" ? do
    "background-color" .= "red"
    "color"            .= "green"

  a .>. (h2 ! "myclass") <> a # "title" ? do
    "background-color" .= "red"
    "color"            .= "green"

  ident "this" ? do
    "position" .= "relative"

  h2 <||> ("foo" |^=| "bar") ? do
    "background-color" .= "red"
    "color"            .= "green"

  a <:> "visited" ? do
    "color" .= "red"

  input ? do
    "background-color" .= "red"
    
    a ? do
      "background-color" .= "green"

    "type" |=| "text" ? do
      "border" .= "none"
      
  fontFace $ do
    "src" .= "url(/assets/fonts/oxygen/Oxygen-Bold.woff2)"
  
  media "screen" $ do
    body ? do
      "background-color" .= "lightgreen"
      
wordlist :: Builder
wordlist = cssBuilder' $ do
  cls "wordlist-view" ? do
    borderLeft ["solid", px 2, hex 0x80B9B4]
    "*" ? fontSize (px 20) -- Change this to change the size of the wordlist.
    fontFamily ["sans-serif"]
    margin     (px 30)
    maxWidth   (px 700)
    width      auto
    background (v "white")
    input ? do
      verticalAlign "middle"
      width         (px 80)
      padding       (px 10)
      margin        [px 5, px 0]
      border        none
      color         (hex 0xF0F0F0)
      background    (hex 0x545454)
    
  cls "wordlist-item" ? do
    font          (px 15) ["woodpecker","sans-serif"]
    display       "inline-block"
    background    (hex 0x80B9B4)
    color         (hex 0xF0F0F0)
    padding       (px 10)
    verticalAlign "middle"
    margin        [px 5, px 10, px 5, px 0]
    span ? do
      after ? do
        cursor     "pointer"
        content    "\\00D7"
        marginLeft (px 5)