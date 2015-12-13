{-# LANGUAGE OverloadedStrings #-}
module Data.Niagra.Selector.Attribute
(
  Attribute(..),
  exists,
  (<=>),
  (<~=>),
  (<|=>),
  (<^=>),
  (<$=>),
  (<*=>),
  renderAttribute,
  buildAttribute,
  parseAttribute
)
where
  
{-
TODO
1. Operator fixity

-}
  
import Control.Applicative
import Data.Monoid

import Data.Either
import Data.Char
  
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
  
import Data.ByteString.Builder
import Data.Attoparsec.ByteString.Char8 as A (takeWhile1,char,satisfy,eitherResult,parse)


import Data.String
-- import Data.List

data Attribute = Existential String -- E[foo]
               | Equality String String -- E[foo="bar"]
               | WhitespaceListContains String String -- E[foo~="bar"]
               | HyphenListContains String String -- E[foo|="en"]
               | BeginsWith String String -- E[foo^="bar"]
               | EndsWith String String -- E[foo$="bar"]
               | Substring String String -- E[foo*="bar"]
  deriving (Eq,Show)
  
instance IsString Attribute where
  fromString = either error id . parseAttribute
  
-- |existentiality attribute selector
exists :: String -> Attribute
exists = Existential

-- |equality attribute selector
infixl 3 <=>
(<=>) :: String -> String -> Attribute
(<=>) = Equality

-- | whitespace list contains
infixl 3 <~=>
(<~=>) :: String -> String -> Attribute
(<~=>) = WhitespaceListContains

-- | hyphen list contains
infixl 3 <|=>
(<|=>) :: String -> String -> Attribute
(<|=>) = HyphenListContains

-- | beginsWith
infixl 3 <^=>
(<^=>) :: String -> String -> Attribute
(<^=>) = BeginsWith

-- | ends with
infixl 3 <$=>
(<$=>) :: String -> String -> Attribute
(<$=>) = EndsWith

-- | substring
infixl 3 <*=>
(<*=>) :: String -> String -> Attribute
(<*=>) = Substring

renderAttribute :: Attribute -> String
renderAttribute = BL.unpack . toLazyByteString . buildAttribute

buildAttribute :: Attribute -> Builder
buildAttribute = f
  where
    bracketed b = char8 '[' <> b <> char8 ']'
    quoted v = char8 '"' <> string8 v <> char8 '"'
    attr e a v = bracketed $ string8 a <> char8 e <> char8 '=' <> quoted v
    f (Existential a) = bracketed $ string8 a
    f (Equality a v) = bracketed $ string8 a <> char8 '=' <> quoted v
    f (WhitespaceListContains a v) = attr '~' a v
    f (HyphenListContains a v) = attr '|' a v
    f (BeginsWith a v) = attr '^' a v
    f (EndsWith a v) = attr '$' a v
    f (Substring a v) = attr '*' a v

parseAttribute :: String -> Either String Attribute
parseAttribute s = either Left f . eitherResult . A.parse parser . B.pack $ s
  where
    -- lexer
    f (attrname,Nothing,Nothing)  = Right $ Existential attrname
    f (attrname,Just "=",Just v)  = Right $ Equality attrname v
    f (attrname,Just "~=",Just v) = Right $ WhitespaceListContains attrname v
    f (attrname,Just "|=",Just v) = Right $ HyphenListContains attrname v
    f (attrname,Just "^=",Just v) = Right $ BeginsWith attrname v
    f (attrname,Just "$=",Just v) = Right $ EndsWith attrname v
    f (attrname,Just "*=",Just v) = Right $ Substring attrname v
    f _                           = Left $ "invalid attribute selector: " ++ s    
    -- grammar
    parser = (,,) <$> attribute <*> separator <*> value
    separator = fmap B.unpack <$> (optional ("=" <|> "~=" <|> "|=" <|> "^=" <|> "$=" <|> "*="))
    value = optional $ char '"' *> (many $ satisfy (/= '"')) <* char '"'
    attribute = B.unpack <$> takeWhile1 p
      where
        -- returns @True@ for -_a-zA-Z
        p = f . ord where f c = c == 45 || c == 95 || c >= 65 && c <= 90 || c >= 97 && c <= 122
    