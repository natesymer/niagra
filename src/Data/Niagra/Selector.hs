{-|
Module      : Data.Niagra.Selector
Description : Selector type and combinator operators
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : stable
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

import Data.Niagra.Builder

import Data.Monoid
import Data.List (intersperse)
import qualified Data.String as S

import Data.Text (Text)
import qualified Data.Text as T

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
              | Media Bool Text [(Text,Builder)]-- ^ @@media@
              | SelectorList [Selector] -- ^ @a, h2, .myclass@
              | Raw Text -- ^ plain string to be rendered-as is in CSS
              | Null -- ^ null string

instance S.IsString Selector where
  fromString = Raw . T.pack
  
-- |Serialize a 'Selector' into a 'Data.Text.Builder'
buildSelector :: Selector -> Builder
buildSelector = f
  where
    between a e b = singleton a <> b <> singleton e
    parens = between '(' ')'
    brackets = between '[' ']'
    curlyb = between '{' '}'
    quoted = between '"' '"' . fromText
    attr e a v = brackets $ fromText a <> e <> singleton '=' <> quoted v
    f Null = mempty
    f (Raw v) = fromText v
    f (Child a b) = f a <> singleton '>' <> f b
    f (Descendant a b) = f a <> singleton ' ' <> f b
    f (ImmediatePrecedence a b) = f a <> singleton '+' <> f b
    f (Precedence a b) = f a <> singleton '~' <> f b
    f (PseudoClass a n (Just b)) = f (PseudoClass a n Nothing) <> parens (f b)
    f (PseudoClass a n Nothing) = f a <> singleton ':' <> fromText n
    f (PseudoType a n (Just b)) = f (PseudoType a n Nothing) <> parens (f b)
    f (PseudoType a n Nothing) = f a <> "::" <> fromText n
    f (Class a cls) = f a <> singleton '.' <> fromText cls
    f (Id a i) = f a <> singleton '#' <> fromText i
    f (SelectorList xs) = mconcat $ intersperse (singleton ',') $ map f xs
    f (AttrExistential s a) = f s <> brackets (fromText a)
    f (AttrEquality s a v) = f s <> attr mempty a v
    f (AttrWhitespaceListContains s a v) = f s <> attr "~" a v
    f (AttrHyphenListContains s a v) = f s <> attr "|" a v
    f (AttrBeginsWith s a v) = f s <> attr "^" a v
    f (AttrEndsWith s a v) = f s <> attr "$" a v
    f (AttrSubstring s a v) = f s <> attr "*" a v
    f FontFace = "@font-face"
    f (Media isOnly v pairs) = "@media"
                             <> singleton ' '
                             <> buildOnly isOnly
                             <> singleton ' '
                             <> fromText v
                             <> buildPairs pairs
      where buildPair (a,b) = fromText a <> singleton ':' <> b
            buildPairs [] = mempty
            buildPairs x = " and " <> parens (mconcat $ map buildPair x)
            buildOnly True = "only"
            buildOnly False = "not"

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
(<||>) s (AttrExistential i a) = AttrExistential (s <||> i) a
(<||>) s (AttrEquality i a b) = AttrEquality (s <||> i) a b
(<||>) s (AttrWhitespaceListContains i a l) = AttrWhitespaceListContains (s <||> i) a l
(<||>) s (AttrHyphenListContains i a l) = AttrHyphenListContains (s <||> i) a l
(<||>) s (AttrBeginsWith i a str) = AttrBeginsWith (s <||> i) a str
(<||>) s (AttrEndsWith i a str) = AttrEndsWith (s <||> i) a str
(<||>) s (AttrSubstring i a str) = AttrSubstring (s <||> i) a str
(<||>) s (PseudoClass i c m) = PseudoClass (s <||> i) c m
(<||>) s (PseudoType i c m) = PseudoType (s <||> i) c m
(<||>) (SelectorList xs) a = SelectorList $ map (\s -> s <||> a) xs
(<||>) s (SelectorList xs) = SelectorList $ map (\a -> s <||> a) xs
(<||>) s (Id i eyeD) = Id (s <||> i) eyeD
(<||>) s (Class i c) = Class (s <||> i) c
-- ensure @media & @font-face aren't nested
(<||>) _ FontFace = FontFace
(<||>) _ m@(Media _ _ _) = m
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

-- parseAttribute :: String -> Either String Attribute
-- parseAttribute s = either Left f . eitherResult . A.parse parser . B.pack $ s
--   where
--     -- lexer
--     f (attrname,Nothing,Nothing)  = Right $ Existential attrname
--     f (attrname,Just "=",Just v)  = Right $ Equality attrname v
--     f (attrname,Just "~=",Just v) = Right $ WhitespaceListContains attrname v
--     f (attrname,Just "|=",Just v) = Right $ HyphenListContains attrname v
--     f (attrname,Just "^=",Just v) = Right $ BeginsWith attrname v
--     f (attrname,Just "$=",Just v) = Right $ EndsWith attrname v
--     f (attrname,Just "*=",Just v) = Right $ Substring attrname v
--     f _                           = Left $ "invalid attribute selector: " ++ s
--     -- grammar
--     parser = (,,) <$> attribute <*> separator <*> value
--     separator = fmap B.unpack <$> (optional ("=" <|> "~=" <|> "|=" <|> "^=" <|> "$=" <|> "*="))
--     value = optional $ char '"' *> (many $ satisfy (/= '"')) <* char '"'
--     attribute = B.unpack <$> takeWhile1 p
--       where
--         -- returns @True@ for -_a-zA-Z
--         p = f . ord where f c = c == 45 || c == 95 || c >= 65 && c <= 90 || c >= 97 && c <= 122
--