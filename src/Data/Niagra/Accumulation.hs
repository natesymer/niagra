{-|
Module      : Data.Niagra.Accumulation
Description : A simple stack implementation
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
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
                    | Accumulation
                        (Accumulation a) -- ^ init
                        a -- ^ last

{-# INLINE push #-}
push :: Accumulation a -> a -> Accumulation a
push = Accumulation

{-# INLINE pop #-}
pop :: Accumulation a -> Maybe (Accumulation a)
pop Empty = Nothing
pop (Accumulation xs _) = Just xs

{-# INLINE popTup #-}
popTup :: Accumulation a -> Maybe (Accumulation a, a)
popTup Empty = Nothing
popTup (Accumulation xs x) = Just (xs, x)

{-# INLINE top #-}
top :: Accumulation a -> Maybe a
top Empty = Nothing
top (Accumulation _ x) = Just x
                
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