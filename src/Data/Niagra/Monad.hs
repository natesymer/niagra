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

import Data.Sequence (Seq(..),(|>))
import qualified Data.Sequence as S (singleton,empty)
import qualified Data.Foldable as F (toList)
import Data.Text (Text)

import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype NiagraT m a = NiagraT (AccumulatorT Block (Selector,(Seq Declaration)) m a)
  deriving (Functor,Applicative,Monad,MonadIO)

-- |Evaluate a NiagraT monadic action.
runNiagraT :: (Monad m) => NiagraT m () -> m (Seq Block)
runNiagraT (NiagraT acc) = snd' <$> runAccumulatorT acc toComplete emptyState S.empty emptyState
  where snd' (_,v,_) = v
        emptyState = (Null,S.empty)
        toComplete (sel,decls) = DeclarationBlock sel $ F.toList decls

-- |Start a root scope.
rootScope :: (Monad m) => Selector -> NiagraT m () -> NiagraT m ()
rootScope sel act@(NiagraT acc) = NiagraT $ (lift $ runNiagraT act) >>= add
  where
    builderBlock = BuilderBlock sel . foldMap buildBlock
    add = addAccumulations . S.singleton . builderBlock
  
-- |Start accumulating a child scope with @sel@.
childScope :: (Monad m) => Selector -> NiagraT m () -> NiagraT m ()
childScope sel (NiagraT acc) = NiagraT $ do
  parent@(parentSel,_) <- getIncomplete
  setIncomplete (parentSel <||> sel,S.empty)
  acc
  complete
  setIncomplete parent

-- |Add a declaration to the 'NiagraT' state.
addDeclaration :: (Monad m) => Text -> Builder -> NiagraT m ()
addDeclaration k v = NiagraT $ incomplete $ \(sel,decls) -> (sel, decls |> decl)
  where decl = Declaration k v

-- |NiagraT monad transformer.
-- newtype NiagraT m a = NiagraT (RWST () (Seq Block) (Seq (Selector,(Seq Declaration))) m a)
--   deriving (Functor,
--             Applicative,
--             Monad,
--             MonadIO,
--             MonadWriter (Seq Block),
--             MonadState (Seq (Selector,(Seq Declaration))))

-- execNiagraT :: (Monad m) => Selector -> NiagraT m () -> m (Seq Block)
-- execNiagraT sel (NiagraT rws) = f <$> runRWST rws () (S.singleton (sel,S.empty))
--   where f (_,_,w) = S.filter (not . isEmpty) w

-- -- |Run an @act@ in a fresh 'NiagraT' state.
-- withNewScope :: (Monad m) => Selector -> NiagraT m () -> NiagraT m ()
-- withNewScope sel act = do
--   get >>= put . push sel
--   act
--   (_ :< xs) <- viewl <$> get
--   put xs
--   where
--     push s st = let ((o,_) :< _) = viewl st
--                 in (o <||> s,S.empty) <| st
--
-- -- |Get a 'Block' from the current 'NiagraT' state.
-- getCurrentBlock :: (Monad m) => NiagraT m Block
-- getCurrentBlock = do
--   ((sel, decls) :< _) <- viewl <$> get
--   return $ DeclarationBlock sel $ F.toList decls
--
-- -- |Add a 'Block' to the 'NiagraT' writer state.
-- addBlock :: (Monad m) => Block -> NiagraT m ()
-- addBlock = tell . S.singleton
--
-- -- |Add a declaration to the 'NiagraT' state.
-- addDeclaration :: (Monad m) => Declaration -> NiagraT m ()
-- addDeclaration decl = get >>= put . f decl
--   where f d st = let ((s,decls) :< xs) = viewl st
--                  in (s,decls |> d) <| xs