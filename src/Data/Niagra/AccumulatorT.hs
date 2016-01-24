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
  evalAccumulatorT,
  complete,
  incomplete,
  getIncomplete,
  setIncomplete,
  getAccumulation,
  accumulate
)
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Niagra.Accumulation

-- | AccumulatorT type.
newtype AccumulatorT s i m a = AccumulatorT {
  runAccumulatorT :: (i -> m s) -- ^ function to turn the incomplete state into an accumulation
                  -> m i -- ^ action to create a new incomplete state
                  -> Accumulation s -- ^ accumulations
                  -> i -- ^ current incomplete state
                  -> m (a, Accumulation s, i) -- ^ result of accumulation
}

instance (Functor m) => Functor (AccumulatorT s i m) where
  fmap f m = AccumulatorT $ \a b c d -> fmap f' $ runAccumulatorT m a b c d
    where f' (a, st', i') = (f a, st', i')

instance (Functor m, Monad m) => Applicative (AccumulatorT s i m) where
  pure a = AccumulatorT $ \fin fi st i -> return (a, st, i)
  (<*>) = ap

instance (Monad m) => Monad (AccumulatorT s i m) where
  fail msg = AccumulatorT $ \_ _ _ _ -> fail msg
  m >>= k = AccumulatorT $ \fin fi st i -> do
    (a, st', i') <- runAccumulatorT m fin fi st i
    runAccumulatorT (k a) fin fi st' i'

instance MonadTrans (AccumulatorT s i) where
  lift m = AccumulatorT $ \_ _ st i -> m >>= return . (,st,i)

instance (MonadIO m) => MonadIO (AccumulatorT s i m) where
  liftIO = lift . liftIO

-- |Evaluates the 'AccumulatorT'.
evalAccumulatorT :: (Monad m)
                 => AccumulatorT s i m a -- ^ 'AccumulatorT' to evaluate
                 -> (i -> m s) -- ^ function to turn the incomplete state into an accumulation
                 -> m i -- ^ action to create a new incomplete state
                 -> m (a, Accumulation s, i) -- ^ result of accumulation
evalAccumulatorT (AccumulatorT f) fin fi = fi >>= f fin fi Empty

-- |Completes the incomplete state and appends it to state collection.
complete :: (Monad m) => AccumulatorT s i m ()
complete = AccumulatorT $ \fin fi st i -> do
  incomp <- fi
  accum <- fin i
  return ((),push st accum,incomp)

-- |Apply a function to the incomplete state.
incomplete :: (Monad m) => (i -> m i) -> AccumulatorT s i m ()
incomplete f = AccumulatorT $ \_ _ st i -> ((),st,) <$> f i

-- |Get the incomplete state.
getIncomplete :: (Monad m) => AccumulatorT s i m i
getIncomplete = AccumulatorT $ \_ _ st i -> return (i, st, i)

-- |Set the incomplete state.
setIncomplete :: (Monad m) => i -> AccumulatorT s i m ()
setIncomplete v = AccumulatorT $ \_ _ st _ -> return ((), st, v)

-- |Get the accumulations.
getAccumulation :: (Monad m) => AccumulatorT s i m (Accumulation s)
getAccumulation = AccumulatorT $ \_ _ st i -> return (st, st, i)

-- |Add a value to the accumulation.
accumulate :: (Monad m) => s -> AccumulatorT s i m ()
accumulate v = AccumulatorT $ \_ _ st i -> return ((),push st v, i)