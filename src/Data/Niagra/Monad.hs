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
  runNiagraT,
  rootScope,
  childScope,
  addDeclaration
)
where
  
import Data.Niagra.Block
import Data.Niagra.Selector
import Data.Niagra.AccumulatorT
import Data.Niagra.Builder

import Data.Text (Text)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as S (singleton,empty)

import Control.Monad.IO.Class
import Control.Monad.Trans.Class

-- TODO: make accumulated state be a Builder.
-- Builders should be able to distinguish between empty & full builders in 
-- O(1) time without building anything.

newtype NiagraT m a = NiagraT (AccumulatorT Block Block m a)
  deriving (Functor,Applicative,Monad,MonadIO)

-- |Evaluate a NiagraT monadic action.
runNiagraT :: (Monad m) => NiagraT m () -> m (Seq Block)
runNiagraT (NiagraT acc) = snd' <$> run
  where snd' (_,v,_) = v
        emptyState = Block Null mempty -- (Null,S.empty)
        run = runAccumulatorT acc return (return emptyState) S.empty emptyState

-- |Start a root scope.
rootScope :: (Monad m) => Selector -> NiagraT m () -> NiagraT m ()
rootScope sel act = NiagraT $ (lift $ runNiagraT act) >>= add
  where
    block = Block sel . foldMap buildBlock
    add = addAccumulations . S.singleton . block
  
-- |Start accumulating a child scope with @sel@.
childScope :: (Monad m) => Selector -> NiagraT m () -> NiagraT m ()
childScope sel (NiagraT acc) = NiagraT $ do
  parent@(Block parentSel _) <- getIncomplete
  setIncomplete $ Block (parentSel <||> sel) mempty
  acc
  complete
  setIncomplete parent

-- |Add a declaration to the 'NiagraT' state.
addDeclaration :: (Monad m) => Text -> Builder -> NiagraT m ()
addDeclaration k v = NiagraT $ incomplete $ \b -> return $ appendDeclaration b k v