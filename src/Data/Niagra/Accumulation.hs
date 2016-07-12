{-|
Module      : Data.Niagra.Accumulation
Description : A simple stack implementation
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : stable
Portability : POSIX
-}

module Data.Niagra.Accumulation
(
  Accumulation(..),
  push,
  pop,
  top,
  fromList
)
where
  
import Data.Foldable

-- |Stack data structure.
data Accumulation a = Empty
                    | Accumulation (Accumulation a) a

{-# INLINE push #-}
-- |Add an element to the end of an 'Accumulation'.
push :: Accumulation a -> a -> Accumulation a
push = Accumulation

{-# INLINE pop #-}
-- |Pop the top off an 'Accumulation'.
pop :: Accumulation a -> Maybe (Accumulation a)
pop Empty = Nothing
pop (Accumulation xs _) = Just xs

{-# INLINE popTup #-}
-- |"unsnoc"
popTup :: Accumulation a -> Maybe (Accumulation a, a)
popTup Empty = Nothing
popTup (Accumulation xs x) = Just (xs, x)

{-# INLINE top #-}
-- |Get the top element on an 'Accumulation'.
top :: Accumulation a -> Maybe a
top Empty = Nothing
top (Accumulation _ x) = Just x
            
-- |Construct an accumulation from a list.    
fromList :: [a] -> Accumulation a
fromList = f Empty
  where f acc [] = acc
        f acc (x:xs) = f (push acc x) xs
                
instance (Show a) => Show (Accumulation a) where
  show = mappend "fromList " . show . toList

instance Foldable Accumulation where
  foldMap f = loop mempty
    where loop acc Empty = acc
          loop acc (Accumulation xs x) = loop (f x `mappend` acc) xs
  null Empty = True
  null _ = False
  
instance (Monoid a) => Monoid (Accumulation a) where
  mempty = Empty
  Empty `mappend` b = b
  a `mappend` Empty = a
  a `mappend` (Accumulation xs x) = Accumulation (a `mappend` xs) x
