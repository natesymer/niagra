{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
  
import Data.Niagra
import Data.Monoid

main :: IO ()
main = example >>= putStrLn . mconcat . map renderRule

example :: (Monad m) => m [Rule]
example = css $ do
  ((a >| h2 .! "myclass") <> (a # "title")) ? do
    declaration "background-color" "red"