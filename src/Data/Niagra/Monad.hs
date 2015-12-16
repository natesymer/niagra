{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Niagra.Monad
(
  NiagraT(..),
  execNiagraT,
  writeBlocks,
  writeDeclarations
)
where
  
import Control.Monad.Trans.Writer
import Control.Monad.IO.Class

import Data.Niagra.Block (Block(..),Declaration(..))

import Data.Either
  
newtype NiagraT m a = NiagraT { niagraWriter :: WriterT [Either Declaration Block] m a }
  deriving (Functor, Applicative, Monad, MonadIO)
  
execNiagraT :: (Monad m) => NiagraT m a -> m [Either Declaration Block]
execNiagraT = execWriterT . niagraWriter

writeBlocks :: (Monad m) => [Block] -> NiagraT m ()
writeBlocks = NiagraT . tell . map Right

writeDeclarations :: (Monad m) => [Declaration] -> NiagraT m ()
writeDeclarations = NiagraT . tell . map Left