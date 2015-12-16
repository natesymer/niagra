{-|
Module      : Data.Niagra.Monad
Description : NiagraT monad transformer
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

'NiagraT' monad transformer; based on 'WriterT'. Stores a state
with type @['Either' 'Declaration' 'Block']@.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Niagra.Monad
(
  NiagraT(..),
  writeBlocks,
  writeDeclarations,
  niagraBlocks,
  niagraDeclarations,
  niagraState
)
where
  
import Data.Niagra.Block
  
import Data.Either
import Control.Monad.Trans.Writer
import Control.Monad.IO.Class

-- |NiagraT monad transformer.
newtype NiagraT m a = NiagraT (WriterT [Either Declaration Block] m a)
  deriving (Functor, Applicative, Monad, MonadIO)
  
execNiagraT :: (Monad m) => NiagraT m a -> m [Either Declaration Block]
execNiagraT (NiagraT w) = filter (either (const True) (not . isEmpty)) <$> execWriterT w

-- |Append 'Block's to the 'NiagraT' state.
writeBlocks :: (Monad m) => [Block] -> NiagraT m ()
writeBlocks = NiagraT . tell . map Right

-- |Append 'Declaration's to the 'NiagraT' state.
writeDeclarations :: (Monad m) => [Declaration] -> NiagraT m ()
writeDeclarations = NiagraT . tell . map Left

-- |Retrieve 'Block's from a 'NiagraT' action.
niagraBlocks :: (Monad m) => NiagraT m () -> m [Block]
niagraBlocks = fmap rights . execNiagraT

-- |Retrieve 'Declaration's from a 'NiagraT' action.
niagraDeclarations :: (Monad m) => NiagraT m () -> m [Declaration]
niagraDeclarations = fmap lefts . execNiagraT

-- |Retrieve both 'Declaration's and 'Block's from a 'NiagraT' action.
niagraState :: (Monad m) => NiagraT m () -> m ([Declaration],[Block])
niagraState = fmap partitionEithers . execNiagraT