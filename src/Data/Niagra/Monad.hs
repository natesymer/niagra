{-|
Module      : Data.Niagra.Monad
Description : NiagraT monad transformer
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

'NiagraT' is a monad transformer based on 'RWST'. It stores no
readonly state, a writeonly state with type @['Block']@, and a
and a readwrite state with type 'Selector'.
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
import qualified Data.Sequence as S (singleton,empty)
import qualified Data.Foldable as F (toList)

import Control.Monad.RWS.Lazy
import Control.Monad.IO.Class

-- |NiagraT monad transformer.
newtype NiagraT m a = NiagraT (RWST () [Block] (Seq (Selector,(Seq Declaration))) m a)
  deriving (Functor,
            Applicative,
            Monad,
            MonadIO,
            MonadState (Seq (Selector,(Seq Declaration))),
            MonadWriter [Block])

-- |Evaluate a NiagraT monadic action.
execNiagraT :: (Monad m) => Selector -> NiagraT m () -> m [Block]
execNiagraT sel (NiagraT rws) = do
  ~(_,_,w) <- runRWST rws () $ S.singleton (sel,S.empty)
  return $ filter (not . isEmpty) w
  
-- |Run an @act@ in a fresh 'NiagraT' state.
withNewScope :: (Monad m) => Selector -> NiagraT m () -> NiagraT m ()
withNewScope sel act = do
  state $ ((),) . push sel
  act
  (_ :< xs) <- viewl <$> get
  put xs
  where
    push s st = (o <||> s,S.empty) <| st where ((o,_) :< _) = viewl st
  
-- |Get a 'Block' from the current 'NiagraT' state.
getCurrentBlock :: (Monad m) => NiagraT m Block
getCurrentBlock = do
  ((sel, decls) :< _) <- viewl <$> get
  return $ DeclarationBlock sel $ F.toList decls

-- |Add a 'Block' to the 'NiagraT' writer state.
addBlock :: (Monad m) => Block -> NiagraT m ()
addBlock blk = tell [blk]

-- |Add a declaration to the 'NiagraT' state.
addDeclaration :: (Monad m) => Declaration -> NiagraT m ()
addDeclaration decl = get >>= put . f decl
  where
    f d st = (s,decls |> d) <| xs
      where ((s,decls) :< xs) = viewl st