{-|
Module      : Data.Niagra.Selector
Description : Selector type and combinator operators
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

Create & manipulate CSS selectors.
-}

{-

IMPORTANT:

* fixity of operators is always based on argument types.

    * Starting with a fixity of 9, subtract the
      number of 'Selector' arguments to get a given function's fixity.

* fixity should be greater than 6 (the (<>) operator's fixity)

* operators must be left-associative

-}

{-# LANGUAGE OverloadedStrings #-}
module Data.Niagra.Selector
(
  -- * Types
  Selector(..),
  -- * Builder
  buildSelector,
  -- * Operators
  (<||>),
  -- ** Selector Operators
  (.>.),
  (.+.),
  (.~.),
  (#),
  (!),
  (<:>),
  (<::>),
  -- ** Attribute Operators
  (|=|),
  (|~=|),
  (||=|),
  (|^=|),
  (|$=|),
  (|*=|),
  cls,
  ident,
  pseudoClass,
  pseudoClass',
  pseudoType,
  pseudoType'
)
where

import Data.Monoid
import Data.List (intersperse)
import qualified Data.String as S

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder

-- |A CSS selector
data Selector = Child Selector Selector -- ^ @a > b@
              | Precedence Selector Selector -- ^ @a ~ b@
              | ImmediatePrecedence Selector Selector -- ^ @a + b@
              | Descendant Selector Selector -- ^ @a b@
              | PseudoClass Selector Text (Maybe Selector) -- ^ @a:hover, a:not(b)@
              | PseudoType Selector Text (Maybe Selector) -- ^ @span::before, span::my-pseudotype(b)@
              | AttrExistential Selector Text -- ^ @h2[foo]@
              | AttrEquality Selector Text Text -- ^ @h2[foo="bar"]@
              | AttrWhitespaceListContains Selector Text Text -- ^ @h2[foo~="bar"]@
              | AttrHyphenListContains Selector Text Text -- ^ @h2[foo|="en"]@
              | AttrBeginsWith Selector Text Text -- ^ @h2[foo^="bar"]@
              | AttrEndsWith Selector Text Text -- ^ @h2[foo$="bar"]@
              | AttrSubstring Selector Text Text -- ^ @h2[foo*="bar"]@
              | Class Selector Text -- ^ @h2.myclass@
              | Id Selector Text -- ^ @a#mylink@
              | FontFace -- ^ @@font-face@
              | SelectorList [Selector] -- ^ @a, h2, .myclass@
              | Raw Text -- ^ plain string to be rendered-as is in CSS
              | Null -- ^ null string
  deriving (Eq,Show)

instance S.IsString Selector where
  fromString = Raw . TL.pack
  
-- |Serialize a 'Selector' into a 'Data.Text.Lazy.Builder'
buildSelector :: Selector -> Builder
buildSelector = f
  where
    between a e b = singleton a <> b <> singleton e
    parens = between '(' ')'
    brackets = between '[' ']'
    curlyb = between '{' '}'
    quoted = between '"' '"' . fromLazyText
    attr e a v = brackets $ fromLazyText a <> e <> "=" <> quoted v
    f Null = mempty
    f (Raw v) = fromLazyText v
    f (Child a b) = f a <> ">" <> f b
    f (Descendant a b) = f a <> " " <> f b
    f (ImmediatePrecedence a b) = f a <> "+" <> f b
    f (Precedence a b) = f a <> "~" <> f b
    f (PseudoClass a n (Just b)) = f (PseudoClass a n Nothing) <> parens (f b)
    f (PseudoClass a n Nothing) = f a <> ":" <> fromLazyText n
    f (PseudoType a n (Just b)) = f (PseudoType a n Nothing) <> parens (f b)
    f (PseudoType a n Nothing) = f a <> "::" <> fromLazyText n
    f (Class a cls) = f a <> "." <> fromLazyText cls
    f (Id a i) = f a <> "#" <> fromLazyText i
    f (SelectorList xs) = mconcat $ intersperse "," $ map f xs
    f (AttrExistential s a) = f s <> brackets (fromLazyText a)
    f (AttrEquality s a v) = f s <> attr mempty a v
    f (AttrWhitespaceListContains s a v) = f s <> attr "~" a v
    f (AttrHyphenListContains s a v) = f s <> attr "|" a v
    f (AttrBeginsWith s a v) = f s <> attr "^" a v
    f (AttrEndsWith s a v) = f s <> attr "$" a v
    f (AttrSubstring s a v) = f s <> attr "*" a v
    f FontFace = "@font-face"
  
-- TODO: Rewrite list concat functionality elsewhere
--       and use monoid instance to implement (<||>)
  
instance Monoid Selector where
  mempty = Null
  mappend Null x = x
  mappend x Null = x
  mappend (SelectorList xs) x = SelectorList $ xs ++ [x]
  mappend x (SelectorList xs) = SelectorList $ x:xs
  mappend a b = SelectorList [a,b]
  mconcat xs = SelectorList xs

{- selector operators -}

-- | Child selector.
infixl 7 .>.
(.>.) :: Selector -- ^ parent
     -> Selector -- ^ child
     -> Selector
(.>.) = Child

-- | immediate precedence.
infixl 7 .+.
(.+.) :: Selector -- ^ first sibling
     -> Selector -- ^ second sibling
     -> Selector
(.+.) = ImmediatePrecedence

-- |Match a pair of contiguous selectors.
infixl 7 .~.
(.~.) :: Selector -- ^ first selector
      -> Selector -- ^ second selector
      -> Selector
(.~.) = Precedence

-- |Match a descendant.
infixl 7 .|.
(.|.) :: Selector -- ^ ancestor
      -> Selector -- ^ descendant
      -> Selector
(.|.) = Descendant

-- |Add an id to a Selector.
infixl 8 #
(#) :: Selector -- ^ 'Selector' to add id to
    -> Text -- ^ id
    -> Selector
(#) = Id

-- |Add a class to a 'Selector'.
infixl 8 !
(!) :: Selector -- ^ 'Selector' to add class to
    -> Text -- ^ class
    -> Selector
(!) = Class

-- |Add a pseudoclass to a 'Selector'. Does not
-- allow for a parenthetial statement to be written
-- as part of the pseudoclass.
infixl 8 <:>
(<:>) :: Selector -- ^ 'Selector' to add pseudoclass to
      -> Text -- ^ pseudoclass
      -> Selector
(<:>) sel n = PseudoClass sel n Nothing

-- |Create a pseudoclass.
pseudoClass :: Text -- ^ the name of the pseudoclass
            -> Maybe Selector -- ^ maybe a parenthetical statement to follow the pseudoclass
            -> Selector
pseudoClass = PseudoClass Null

pseudoClass' :: Text -> Selector
pseudoClass' = flip pseudoClass Nothing

-- |Add a pseudotype to a 'Selector'. Does not
-- allow for a parenthetial statement to be written
-- as part of the pseudoclass.
infixl 8 <::>
(<::>) :: Selector -- ^ 'Selector' to add pseudotype to
       -> Text -- ^ pseudotype
       -> Selector
(<::>) sel n = PseudoType sel n Nothing

-- |Create a pseudotype.
pseudoType :: Text -- ^ the name of the pseudotype
           -> Maybe Selector -- ^ maybe a parenthetical statement to follow the pseudotype
           -> Selector -- ^
pseudoType = PseudoType Null

pseudoType' :: Text -> Selector
pseudoType' = flip pseudoType Nothing

-- |Add aspect operator. Used to construct larger selectors
-- from smaller ones. Often types, 'Selector's are constructed
-- with the first argument set to 'Null', eg @Class Null "myclass"@.
-- You can use this operator to create a selector like this: @h2.myclass@
-- by doing something like @(Raw "h2") \<||\> (Class Null "myclass")@ (which
-- is equivalent to @Class (Raw "h2") "myclass"@).
infixl 7 <||>
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
(<||>) s (SelectorList xs) = SelectorList $ map (\a -> s <||> a) xs
(<||>) s (Id _ i) = Id s i
(<||>) s (Class _ c) = Class s c
-- lineage case
(<||>) s s' = Descendant s s'

-- |Create a CSS @class@.
cls :: Text -- ^ name of the @class@
    -> Selector
cls = Class Null

-- |Create an CSS @id@.
ident :: Text -- ^ name of the @id@
      -> Selector
ident = Id Null

{- By-Attribute selector operators -}

-- |Equality.
infixl 9 |=|
(|=|) :: Text -- ^ attribute name
      -> Text -- ^ desired value to test for equality
      -> Selector
(|=|) = AttrEquality Null

-- |Whitespace-separated list contains.
infixl 9 |~=|
(|~=|) :: Text -- ^ attribute name
       -> Text -- ^ value to be found in whitespace-separated list
       -> Selector
(|~=|) = AttrWhitespaceListContains Null

-- |Hyphen-separated list contains.
infixl 9 ||=|
(||=|) :: Text -- ^ attribute name
       -> Text -- ^ value to be found in hyphen-separated list
       -> Selector
(||=|) = AttrHyphenListContains Null

-- |Begins with.
infixl 9 |^=|
(|^=|) :: Text -- ^ attribute name
       -> Text -- ^ string beginning
       -> Selector
(|^=|) = AttrBeginsWith Null

-- |Ends with.
infixl 9 |$=|
(|$=|) :: Text -- ^ attribute name
       -> Text -- ^ string ending
       -> Selector
(|$=|) = AttrEndsWith Null

-- |Substring.
infixl 9 |*=|
(|*=|) :: Text -- ^ attribute name
       -> Text -- ^ substring in attribute
       -> Selector
(|*=|) = AttrSubstring Null