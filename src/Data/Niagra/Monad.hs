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
  execNiagraT,
  withNewScope,
  getCurrentBlock,
  addBlock,
  addDeclaration
)
where
  
import Data.Niagra.Block
import Data.Niagra.Selector

import Data.Sequence (Seq(..),viewl,ViewL(..),(<|),(|>))
import qualified Data.Sequence as S (singleton,empty,filter)
import qualified Data.Foldable as F (toList)

import Control.Monad.RWS.Lazy

-- |NiagraT monad transformer.
newtype NiagraT m a = NiagraT (RWST () (Seq Block) (Seq (Selector,(Seq Declaration))) m a)
  deriving (Functor,
            Applicative,
            Monad,
            MonadIO,
            MonadWriter (Seq Block),
            MonadState (Seq (Selector,(Seq Declaration))))

-- |Evaluate a NiagraT monadic action.
execNiagraT :: (Monad m) => Selector -> NiagraT m () -> m [Block]
execNiagraT sel (NiagraT rws) = f <$> runRWST rws () (S.singleton (sel,S.empty))
  where f ~(_,_,w) = F.toList $ S.filter (not . isEmpty) w
  
-- |Run an @act@ in a fresh 'NiagraT' state.
withNewScope :: (Monad m) => Selector -> NiagraT m () -> NiagraT m ()
withNewScope sel act = do
  get >>= put . push sel
  act
  (_ :< xs) <- viewl <$> get
  put xs
  where
    push s st = let (~(o,_) :< _) = viewl st
                in (o <||> s,S.empty) <| st
                
-- |Get a 'Block' from the current 'NiagraT' state.
getCurrentBlock :: (Monad m) => NiagraT m Block
getCurrentBlock = do
  (~(sel, decls) :< _) <- viewl <$> get
  return $ DeclarationBlock sel $ F.toList decls

-- |Add a 'Block' to the 'NiagraT' writer state.
addBlock :: (Monad m) => Block -> NiagraT m ()
addBlock = tell . S.singleton

-- |Add a declaration to the 'NiagraT' state.
addDeclaration :: (Monad m) => Declaration -> NiagraT m ()
addDeclaration decl = get >>= put . f decl
  where f d st = let (~(s,decls) :< xs) = viewl st
                 in (s,decls |> d) <| xs