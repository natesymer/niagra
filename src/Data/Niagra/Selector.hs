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
  (#),
  (.!),
  renderSelector
)
where
  
import Data.String
import Data.List

instance IsString Selector where
  fromString = Raw
  
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
              | ImmediatePrecedence Selector Selector
              | Precedence Selector Selector
              | Pseudoclassed Selector PseudoClass -- eg "a:hover"
              | Pseudotyped Selector PseudoType -- Example: span::before
              | Class Selector String -- Example: h2.myclass
              | Id Selector String -- Example: a#mylink
              | SelectorList [Selector] -- Example: a, h2, .myclass
              | Raw String -- Just a plain string
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

-- | Child selector.
infixl 2 >|
(>|) :: Selector -- ^ parent
     -> Selector -- ^ child
     -> Selector
(>|) = Child

-- | immediate precedence
infixl 2 +|
(+|) :: Selector -- ^ first sibling
     -> Selector -- ^ second sibling
     -> Selector
(+|) = ImmediatePrecedence

infixl 2 ~|
(~|) :: Selector -> Selector -> Selector
(~|) = Precedence

infixl 2 #
(#) :: Selector -> String -> Selector
(#) = Id

infixl 3 .!
(.!) :: Selector -> String -> Selector
(.!) = Class

renderSelector :: Selector -> String
renderSelector = f
  where
    f Null = ""
    f (Raw v) = v
    f (Child a b) = mconcat [f a, " > ", f b]
    f (ImmediatePrecedence a b) = mconcat [f a, " + ", f b]
    f (Precedence a b) = mconcat [f a, " ~ ", f b]
    f (Pseudoclassed a (PseudoClass n Nothing)) = mconcat [f a, ":", n]
    f (Pseudoclassed a (PseudoClass n (Just b))) = mconcat [f a,":",n,"(",f b,")"]
    f (Pseudotyped a (PseudoType n Nothing)) = mconcat [f a, "::", n]
    f (Pseudotyped a (PseudoType n (Just b))) = mconcat [f a,"::",n,"(",f b,")"]
    f (Class a cls) = mconcat [f a, ".", cls]
    f (Id a i) = mconcat [f a, ".", i]
    f (SelectorList xs) = mconcat $ intersperse "," $ map f xs