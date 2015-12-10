module Data.Niagra.Render
(
  renderSelector
)
where
  
import Data.Niagra.Selector
import Data.List (intersperse)

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