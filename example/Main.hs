{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
  
import Data.Niagra
import Data.Monoid
  
testSelector :: Selector
testSelector = (a >| h2 .! "myclass") <> (a # "title")

main :: IO ()
main = putStrLn $ renderSelector $ testSelector