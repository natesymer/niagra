{-# LANGUAGE OverloadedStrings #-}

module Data.Niagra.Properties.Font
(
  font,
  fontFamily,
  fontWeight,
  fontSize
)
where
  
import Data.Niagra.Monad
import Data.Niagra.DSL
import Data.Niagra.Value

import Data.Monoid
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int (decimal)

font :: (Monad m) => Builder -> [Builder] -> NiagraT m ()
font size fam = declaration "font" $ build [size, commaSeparate fam]

fontFamily :: (Monad m) => [Builder] -> NiagraT m ()
fontFamily = declaration "font-family" . commaSeparate

fontWeight :: (Monad m) => Integer -> NiagraT m ()
fontWeight = declaration "font-weight" . decimal

fontSize :: (Monad m) => Builder -> NiagraT m ()
fontSize = declaration "font-size"

{- INTERNAL -}

commaSeparate :: [Builder] -> Builder
commaSeparate = f mempty
  where
    f a [] = a
    f a (x:xs)
      | a == mempty = f x xs
      | otherwise = f (a <> singleton ',' <> x) xs

{-
TODO 
font-feature-settings	Allows control over advanced typographic features in OpenType fonts	3
font-kerning	Controls the usage of the kerning information (how letters are spaced)	3
font-language-override	Controls the usage of language-specific glyphs in a typeface	3
font-size-adjust	Preserves the readability of text when font fallback occurs	3
font-stretch	Selects a normal, condensed, or expanded face from a font family	3
font-style	Specifies the font style for text	1
font-synthesis	Controls which missing typefaces (bold or italic) may be synthesized by the browser	3
font-variant	Specifies the font variant. Default value is "normal". See font-variant for possible values
-}