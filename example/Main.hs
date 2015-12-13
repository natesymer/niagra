{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
  
import Data.Niagra
import Data.Monoid

main :: IO ()
main = putStrLn example

example :: String
example = css' $ do
  a # "title" ? do
    declaration "background-color" "red"
    declaration "color" "green"
    
  a >| (h2 .! "myclass") <> a # "title" ? do
    declaration "background-color" "red"
    declaration "color" "green"
    
  "h2" <||> ("foo" <^=> "bar") ? do
    declaration "background-color" "red"
    declaration "color" "green"