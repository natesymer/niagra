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

{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
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
  
import Data.Niagra.Block
import Data.Niagra.Selector
import Data.Niagra.AccumulatorT
import Data.Niagra.Accumulation
import Data.Niagra.Builder

import Data.Text (Text)

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Identity

-- TODO: make accumulated state be a Builder.

newtype NiagraT m a = NiagraT (AccumulatorT Block Block m a)
  deriving (Functor,Applicative,Monad,MonadIO)
  
type Niagra a = NiagraT Identity a

-- |Evaluate a 'NiagraT' monadic action.
runNiagraT :: (Monad m) => NiagraT m () -> m (Accumulation Block)
runNiagraT (NiagraT acc) = snd' <$> run
  where snd' (_,v,_) = v
        emptyState = return $ Block Null mempty
        run = evalAccumulatorT acc return emptyState
        
-- |Evaluate a 'Niagra' monadic action.
runNiagra :: Niagra () -> Accumulation Block
runNiagra = runIdentity . runNiagraT

-- |Start a root scope.
rootScope :: (Monad m) => Selector -> NiagraT m () -> NiagraT m ()
rootScope sel act = NiagraT $ (lift $ runNiagraT act) >>= add
  where add = accumulate . Block sel . foldMap buildBlock
  
-- |Start accumulating a child scope with @sel@.
childScope :: (Monad m) => Selector -> NiagraT m () -> NiagraT m ()
childScope sel (NiagraT acc) = NiagraT $ do
  parent@(Block parentSel _) <- getIncomplete
  setIncomplete $ Block (parentSel <||> sel) mempty
  acc
  complete
  setIncomplete parent

-- |Add a declaration to the 'NiagraT' state.
declaration :: (Monad m) => Text -> Builder -> NiagraT m ()
declaration k v = NiagraT $ incomplete $ \b -> return $ appendDeclaration b k v

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