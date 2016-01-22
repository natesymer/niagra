{-|
Module      : Data.Niagra.AccumulatorT
Description : Monad for accumulating state
Copyright   : (c) Nathaniel Symer, 2015
License     : MIT
Maintainer  : nate@symer.io
Stability   : experimental
Portability : POSIX

The @'AccumulatorT' s i m a@ monad accumulates values that are created
incrementally. Based on 'RWST'.

'AccumulatorT' was designed to create a stronger link between a
read/write state and a writeonly state.

-}

{-# LANGUAGE TupleSections #-}

module Data.Niagra.AccumulatorT
(
  AccumulatorT(..),
  complete,
  incomplete,
  getIncomplete,
  setIncomplete,
  getAccumulations,
  addAccumulations,
  accumulate
)
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Sequence

-- | AccumulatorT type.
newtype AccumulatorT s i m a = AccumulatorT {
  runAccumulatorT :: (i -> m s) -- ^ function to turn the incomplete state into an accumulation
                  -> m i -- ^ action to create a new incomplete state
                  -> Seq s -- ^ accumulations
                  -> i -- ^ current incomplete state
                  -> m (a, Seq s, i) -- ^ result of accumulation
}

instance (Functor m) => Functor (AccumulatorT s i m) where
  fmap f m = AccumulatorT $ \a b c d -> fmap f' $ runAccumulatorT m a b c d
    where f' (a, sq', i') = (f a, sq', i')

instance (Functor m, Monad m) => Applicative (AccumulatorT s i m) where
  pure a = AccumulatorT $ \fin fi sq i -> return (a, sq, i)
  (<*>) = ap

instance (Monad m) => Monad (AccumulatorT s i m) where
  fail msg = AccumulatorT $ \_ _ _ _ -> fail msg
  m >>= k = AccumulatorT $ \fin fi sq i -> do
    (a, sq', i') <- runAccumulatorT m fin fi sq i
    runAccumulatorT (k a) fin fi sq' i'

instance MonadTrans (AccumulatorT s i) where
  lift m = AccumulatorT $ \_ _ sq i -> m >>= return . (,sq,i)

instance (MonadIO m) => MonadIO (AccumulatorT s i m) where
  liftIO = lift . liftIO
    
-- |Completes the incomplete state and appends it to state collection.
complete :: (Monad m) => AccumulatorT s i m ()
complete = AccumulatorT $ \fin fi sq i -> do
  incomp <- fi
  accum <- fin i
  return ((),sq |> accum,incomp)

-- |Apply a function to the incomplete state.
incomplete :: (Monad m) => (i -> m i) -> AccumulatorT s i m ()
incomplete f = AccumulatorT $ \_ _ sq i -> ((),sq,) <$> f i

-- |Get the incomplete state.
getIncomplete :: (Monad m) => AccumulatorT s i m i
getIncomplete = AccumulatorT $ \_ _ sq i -> return (i, sq, i)

-- |Set the incomplete state.
setIncomplete :: (Monad m) => i -> AccumulatorT s i m ()
setIncomplete v = AccumulatorT $ \_ _ sq _ -> return ((), sq, v)

-- |Get the accumulations.
getAccumulations :: (Monad m) => AccumulatorT s i m (Seq s)
getAccumulations = AccumulatorT $ \_ _ sq i -> return (sq, sq, i)

addAccumulations :: (Monad m) => Seq s -> AccumulatorT s i m ()
addAccumulations acc = AccumulatorT $ \_ _ sq i -> return ((),sq >< acc, i)

accumulate :: (Monad m) => s -> AccumulatorT s i m ()
accumulate v = AccumulatorT $ \_ _ sq i -> return ((),sq |> v, i)