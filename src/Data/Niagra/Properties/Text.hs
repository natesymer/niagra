{-# LANGUAGE OverloadedStrings #-}

module Data.Niagra.Properties.Text
(
  lineHeight,
  textAlign
)
where
  
import Data.Niagra.Monad
import Data.Niagra.Value
import Data.Niagra.Builder
  
lineHeight :: (Monad m) => Builder -> NiagraT m ()
lineHeight = declaration "line-height" . build

textAlign :: (Monad m) => Builder -> NiagraT m ()
textAlign = declaration "text-align"
  
{-

TODO

hanging-punctuation	Specifies whether a punctuation character may be placed outside the line box	3
hyphens	Sets how to split words to improve the layout of paragraphs	3
letter-spacing	Increases or decreases the space between characters in a text	1
line-break	Specifies how/if to break lines	3
overflow-wrap	Specifies whether or not the browser may break lines within words in order to prevent overflow (when a string is too long to fit its containing box)	3
tab-size	Specifies the length of the tab-character	3
text-align-last	Describes how the last line of a block or a line right before a forced line break is aligned when text-align is "justify"	3
text-combine-upright	Specifies the combination of multiple characters into the space of a single character	3
text-indent	Specifies the indentation of the first line in a text-block	1
text-justify	Specifies the justification method used when text-align is "justify"	3
text-transform	Controls the capitalization of text	1
white-space	Specifies how white-space inside an element is handled	1
word-break	Specifies line breaking rules for non-CJK scripts	3
word-spacing	Increases or decreases the space between words in a text	1
word-wrap	Allows long, unbreakable words to be broken and wrap to the next line	3


Text Decoration Properties

text-decoration	Specifies the decoration added to text	1
text-decoration-color	Specifies the color of the text-decoration	3
text-decoration-line	Specifies the type of line in a text-decoration	3
text-decoration-style	Specifies the style of the line in a text decoration	3
text-shadow	Adds shadow to text	3
text-underline-position	Specifies the position of the underline which is set using the text-decoration property

-}