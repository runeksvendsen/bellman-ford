{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Queue
( -- * Data types
  MQueue
  -- * Construction
, new
, singleton
  -- * Modification
, enqueue
, dequeue
  -- * Utility
, toList
)
where

import           Data.Mutable                       (PrimMonad, PrimState)
import qualified Data.Mutable                       as Mut
import qualified Data.Vector                        as Vec
import qualified Control.Monad.ST                   as ST


newtype MQueue s a = MQueue { getDeque :: Mut.Deque Vec.MVector s a }

new :: (PrimMonad m)
    => m (MQueue (PrimState m) a)
new = MQueue <$> Mut.newColl

singleton
    :: (PrimMonad m)
    => a
    -> m (MQueue (PrimState m) a)
singleton item = do
    queue <- new
    enqueue queue item
    return queue

enqueue
    :: (PrimMonad m)
    => MQueue (PrimState m) a
    -> a
    -> m ()
enqueue (MQueue deque) = Mut.pushFront deque

dequeue
    :: (PrimMonad m)
    => MQueue (PrimState m) a
    -> m (Maybe a)
dequeue = Mut.popBack . getDeque

toList
    :: (PrimMonad m)
    => MQueue (PrimState m) a
    -> m [a]
toList = Mut.toList . getDeque
