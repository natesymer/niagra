{-# LANGUAGE OverloadedStrings #-}
module Data.Niagra.Selector
(
  -- * Types
  Selector(..),
  PseudoType(..),
  PseudoClass(..),
  -- * Operators
  (>|),
  (+|),
  (~|),
  (<||>),
  (#),
  (!),
  (<:>),
  (<::>),
  any,
  cls,
  ident,
  renderSelector,
  buildSelector
)
where

import Data.Niagra.Buildable
import Data.Niagra.Selector.Attribute
  
import Data.String

import Data.Monoid
import Data.List (intersperse)
import Prelude hiding (any)

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BL

instance IsString Selector where
  fromString = Raw
  
instance Buildable Selector where
  build = buildSelector
  
data PseudoClass = PseudoClass {
  pcName :: String,
  pcSelector :: Maybe Selector
} deriving (Eq,Show)
  
data PseudoType = PseudoType {
  ptName :: String,
  ptSelector :: Maybe Selector
} deriving (Eq,Show)

--------------------------------------------------------------------------------
data Selector = Child Selector Selector
              | ImmediatePrecedence Selector Selector -- Example: a + b
              | Descendant Selector Selector -- E F an F element descendant of an E element	1
              | Precedence Selector Selector -- Example: a ~ b
              | Pseudoclassed Selector PseudoClass -- Example: a:hover
              | Pseudotyped Selector PseudoType -- Example: span::before
              | Class Selector String -- Example: h2.myclass
              | Id Selector String -- Example: a#mylink
              | SelectorList [Selector] -- Example: a, h2, .myclass
              | Raw String -- Just a plain string
              | ByAttribute Selector Attribute -- See 'Attribute'
              | Null
  deriving (Eq,Show)

instance Monoid Selector where
  mempty = Null
  mappend Null x = x
  mappend x Null = x
  mappend (SelectorList xs) x = SelectorList $ x:xs 
  mappend x (SelectorList xs) = SelectorList $ x:xs
  mappend a b = SelectorList [a,b]
  mconcat xs = SelectorList xs

class Subbable a where
  appendToSelector :: a -> Selector -> Selector
  
instance Subbable Attribute where
  appendToSelector = flip ByAttribute
  
instance Subbable PseudoClass where
  appendToSelector = flip Pseudoclassed
  
instance Subbable PseudoType where
  appendToSelector = flip Pseudotyped
  
instance Subbable Selector where
  appendToSelector = flip Descendant

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

infixl 4 <||>
(<||>) :: Selector -> Attribute -> Selector
(<||>) = ByAttribute

infixl 4 #
(#) :: Selector -> String -> Selector
(#) = Id

infixl 4 !
(!) :: Selector -> String -> Selector
(!) = Class

infixl 4 <:>
(<:>) :: Selector -> PseudoClass -> Selector
(<:>) = Pseudoclassed

infixl 4 <::>
(<::>) :: Selector -> PseudoType -> Selector
(<::>) = Pseudotyped

any :: Selector
any = Raw "*"

cls :: String -> Selector
cls = Class Null

ident :: String -> Selector
ident = Id Null

renderSelector :: Selector -> String
renderSelector = BL.unpack . toLazyByteString . buildSelector

buildSelector :: Selector -> Builder
buildSelector = f
  where
    parens b = char8 '(' <> b <> char8 ')'
    f Null = string8 ""
    f (Raw v) = string8 v
    f (Child a b) = f a <> char8 '>' <> f b
    f (Descendant a b) = f a <> char8 ' ' <> f b
    f (ImmediatePrecedence a b) = f a <> char8 '+' <> f b
    f (Precedence a b) = f a <> char8 '~' <> f b
    f (Pseudoclassed a (PseudoClass n Nothing)) = f a <> char8 ':' <> string8 n
    f (Pseudoclassed a (PseudoClass n (Just b))) = f a <> char8 ':' <> string8 n <> parens (f b)
    f (Pseudotyped a (PseudoType n Nothing)) = f a <> string8 "::" <> string8 n
    f (Pseudotyped a (PseudoType n (Just b))) = f a <> string8 "::" <> string8 n <> parens (f b)
    f (Class a cls) = f a <> char8 '.' <> string8 cls
    f (Id a i) = f a <> char8 '#' <> string8 i
    f (SelectorList xs) = mconcat $ map f (intersperse (Raw ",") xs)
    f (ByAttribute s attr) = f s <> buildAttribute attr
