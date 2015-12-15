{-# LANGUAGE OverloadedStrings #-}
module Data.Niagra.Selector
(
  -- * Types
  Selector(..),
  -- * Operators
  (<||>),
  (>|),
  (+|),
  (~|),
  (#),
  (!),
  (<:>),
  (<::>),
  (<=>),
  (<~=>),
  (<|=>),
  (<^=>),
  (<$=>),
  (<*=>),
  any,
  cls,
  ident,
  fontFace,
  pseudoClass,
  pseudoType
)
where

import Data.Niagra.Buildable
  
import Data.String

import Data.Monoid
import Data.List (intersperse)
import Prelude hiding (any)

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BL

data Selector = Child Selector Selector
              | Precedence Selector Selector -- Example: a ~ b
              | ImmediatePrecedence Selector Selector -- Example: a + b
              | Descendant Selector Selector -- E F an F element descendant of an E element	1 
              | PseudoClass Selector String (Maybe Selector) -- Example: a:hover or a:not(b)
              | PseudoType Selector String (Maybe Selector) -- Example: span::before or span::my-pseudotype(b)
              | AttrExistential Selector String -- E[foo]
              | AttrEquality Selector String String -- E[foo="bar"]
              | AttrWhitespaceListContains Selector String String -- E[foo~="bar"]
              | AttrHyphenListContains Selector String String -- E[foo|="en"]
              | AttrBeginsWith Selector String String -- E[foo^="bar"]
              | AttrEndsWith Selector String String -- E[foo$="bar"]
              | AttrSubstring Selector String String -- E[foo*="bar"]
              | Class Selector String -- Example: h2.myclass
              | Id Selector String -- Example: a#mylink
              | FontFace -- @font-face
              | SelectorList [Selector] -- Example: a, h2, .myclass
              | Raw String -- Just a plain string
              | Null
  deriving (Eq,Show)

instance IsString Selector where
  fromString = Raw
  
instance Buildable Selector where
  build = f
    where
      between a e b = char8 a <> b <> char8 e
      parens = between '(' ')'
      bracketed = between '[' ']'
      curlyb = between '{' '}'
      quoted = between '"' '"' . string8
      attr e a v = bracketed $ string8 a <> char8 e <> char8 '=' <> quoted v
      f Null = string8 ""
      f (Raw v) = string8 v
      f (Child a b) = f a <> char8 '>' <> f b
      f (Descendant a b) = f a <> char8 ' ' <> f b
      f (ImmediatePrecedence a b) = f a <> char8 '+' <> f b
      f (Precedence a b) = f a <> char8 '~' <> f b
      f (PseudoClass a n (Just b)) = f a <> char8 ':' <> string8 n <> parens (f b)
      f (PseudoClass a n Nothing) = f a <> char8 ':' <> string8 n
      f (PseudoType a n (Just b)) = f a <> string8 "::" <> string8 n <> parens (f b)
      f (PseudoType a n Nothing) = f a <> string8 "::" <> string8 n
      f (Class a cls) = f a <> char8 '.' <> string8 cls
      f (Id a i) = f a <> char8 '#' <> string8 i
      f (SelectorList xs) = mconcat $ map f (intersperse (Raw ",") xs)
      f (AttrExistential s a) = f s <> (bracketed $ string8 a)
      f (AttrEquality s a v) = f s <> bracketed (string8 a <> char8 '=' <> quoted v)
      f (AttrWhitespaceListContains s a v) = f s <> attr '~' a v
      f (AttrHyphenListContains s a v) = f s <> attr '|' a v
      f (AttrBeginsWith s a v) = f s <> attr '^' a v
      f (AttrEndsWith s a v) = f s <> attr '$' a v
      f (AttrSubstring s a v) = f s <> attr '*' a v
      f FontFace = "@font-face"
  
-- TODO: write @instance Alternative Selector where ...@
-- use this alternative instance to OR Selectors for the following syntax:
-- a,h2,h4{..}
  
instance Monoid Selector where
  mempty = Null
  mappend Null x = x
  mappend x Null = x
  mappend (SelectorList xs) x = SelectorList $ x:xs
  mappend x (SelectorList xs) = SelectorList $ x:xs
  mappend a b = SelectorList [a,b]
  mconcat xs = SelectorList xs

{- selector operators -}

-- | Child selector.
infixl 5 >|
(>|) :: Selector -- ^ parent
     -> Selector -- ^ child
     -> Selector
(>|) = Child

-- | immediate precedence
infixl 5 +|
(+|) :: Selector -- ^ first sibling
     -> Selector -- ^ second sibling
     -> Selector
(+|) = ImmediatePrecedence

infixl 5 ~|
(~|) :: Selector -> Selector -> Selector
(~|) = Precedence

infixl 4 #
(#) :: Selector -> String -> Selector
(#) = Id

infixl 4 !
(!) :: Selector -> String -> Selector
(!) = Class

infixl 4 <:>
(<:>) :: Selector -> String -> Selector
(<:>) sel n = PseudoClass sel n Nothing

pseudoClass :: String -> Maybe Selector -> Selector
pseudoClass = PseudoClass Null

infixl 4 <::>
(<::>) :: Selector -> String -> Selector
(<::>) sel n = PseudoType sel n Nothing

pseudoType :: String -> Maybe Selector -> Selector
pseudoType = PseudoType Null

-- |Add aspect operator. Used to construct larger selectors
-- from smaller ones. Often types, 'Selector's are constructed
-- with the first argument set to 'Null', eg @Class Null "myclass"@.
-- You can use this operator to create a selector like this: @h2.myclass@
-- by doing something like @(Raw "h2") <||> (Class Null "myclass")@ (which
-- is equivalent to @Class (Raw "h2") "myclass").
infixl 4 <||>
(<||>) :: Selector -- selector to add aspect to
       -> Selector -- aspect
       -> Selector
-- Null case
(<||>) s Null = s
(<||>) Null s = s
-- "trait" cases (aspect modifies selector)
(<||>) s (AttrExistential _ a) = AttrExistential s a
(<||>) s (AttrEquality _ a b) = AttrEquality s a b
(<||>) s (AttrWhitespaceListContains _ a l) = AttrWhitespaceListContains s a l
(<||>) s (AttrHyphenListContains _ a l) = AttrHyphenListContains s a l
(<||>) s (AttrBeginsWith _ a str) = AttrBeginsWith s a str
(<||>) s (AttrEndsWith _ a str) = AttrEndsWith s a str
(<||>) s (AttrSubstring _ a str) = AttrSubstring s a str
(<||>) s (PseudoClass _ c m) = PseudoClass s c m
(<||>) s (PseudoType _ c m) = PseudoType s c m
(<||>) (SelectorList xs) a = SelectorList $ map (\s -> s <||> a) xs
(<||>) s (Id _ i) = Id s i
(<||>) s (Class _ c) = Class s c
-- lineage case
(<||>) s s' = Descendant s s'


any :: Selector
any = "*"

cls :: String -> Selector
cls = Class Null

ident :: String -> Selector
ident = Id Null

fontFace :: Selector
fontFace = FontFace

{- By-Attribute selector operators -}

-- |equality attribute selector
infixl 3 <=>
(<=>) :: String -> String -> Selector
(<=>) = AttrEquality Null

-- | whitespace list contains
infixl 3 <~=>
(<~=>) :: String -> String -> Selector
(<~=>) = AttrWhitespaceListContains Null

-- | hyphen list contains
infixl 3 <|=>
(<|=>) :: String -> String -> Selector
(<|=>) = AttrHyphenListContains Null

-- | beginsWith
infixl 3 <^=>
(<^=>) :: String -> String -> Selector
(<^=>) = AttrBeginsWith Null

-- | ends with
infixl 3 <$=>
(<$=>) :: String -> String -> Selector
(<$=>) = AttrEndsWith Null

-- | substring
infixl 3 <*=>
(<*=>) :: String -> String -> Selector
(<*=>) = AttrSubstring Null