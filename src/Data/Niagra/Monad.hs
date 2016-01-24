{-|
Module      : Data.Niagra.Monad
Description : NiagraT monad transformer
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

'NiagraT' is a monad transformer based on 'RWST'. It stores a
combination of total CSS rendering state (blocks) in the writer state
& a state of the currently rendering block in the readwrite state.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Niagra.Monad
(
  NiagraT(..),
  Niagra(..),
  runNiagraT,
  runNiagra,
  -- * Monadic Operations
  rootScope,
  childScope,
  declaration,
  (?),
  (.=)
)
where

import Data.Niagra.Selector
import Data.Niagra.AccumulatorT
import Data.Niagra.Builder

import Data.Text (Text)
import Data.Monoid
import Data.Foldable

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Identity

newtype NiagraT m a = NiagraT (AccumulatorT Builder (Selector,Builder) m a)
  deriving (Functor,Applicative,Monad,MonadIO)

type Niagra a = NiagraT Identity a

-- |Evaluate a 'NiagraT' monadic action.
runNiagraT :: (Monad m) => NiagraT m () -> m Builder
runNiagraT (NiagraT acc) = fold . snd' <$> run
  where snd' (_,v,_) = v
        emptyState = return (Null,mempty)
        run = evalAccumulatorT acc (return . buildIncomp) emptyState
        buildIncomp (_,EmptyBuilder) = EmptyBuilder
        buildIncomp (s,b) = buildSelector s <> singleton '{' <> b <> singleton '}'
        
-- |Evaluate a 'Niagra' monadic action.
runNiagra :: Niagra () -> Builder
runNiagra = runIdentity . runNiagraT

-- |Start a root scope.
rootScope :: (Monad m) => Selector -> NiagraT m () -> NiagraT m ()
rootScope sel act = NiagraT $ (lift $ runNiagraT act) >>= accumulate . f
  where f EmptyBuilder = EmptyBuilder
        f b = buildSelector sel <> singleton '{' <> b <> singleton '}'

-- |Start accumulating a child scope with @sel@.
childScope :: (Monad m) => Selector -> NiagraT m () -> NiagraT m ()
childScope sel (NiagraT acc) = NiagraT $ do
  old@(parent,_) <- getIncomplete
  setIncomplete (parent <||> sel, mempty)
  acc
  complete
  setIncomplete old

-- |Add a declaration to the 'NiagraT' state.
declaration :: (Monad m) => Text -> Builder -> NiagraT m ()
declaration k v = NiagraT $ incomplete (uncurry f)
  where
    f s EmptyBuilder = let b' = fromText k <> singleton ':' <> v
                       in return (s,b')
    f s b = let b' = b <> singleton ';' <> fromText k <> singleton ':' <> v
            in return (s,b')

-- |Operator equivalent of 'block'.
infix 0 ?
{-# INLINE (?) #-}
(?) :: (Monad m) => Selector -> NiagraT m () -> NiagraT m ()
(?) = childScope

-- |Operator equivalent of 'declaration'.
infix 1 .=
{-# INLINE (.=) #-}
(.=) :: (Monad m) => Text -> Builder -> NiagraT m ()
(.=) = declaration