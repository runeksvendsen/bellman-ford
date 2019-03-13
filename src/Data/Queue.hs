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
, mstream
, mstreamR
, S.Stream
)
where

import           Data.Mutable                       (PrimMonad, PrimState)
import qualified Data.Mutable                       as Mut
import           Data.Vector.Generic.Mutable.Base   (MVector)
import qualified Data.Vector                        as Vec
import qualified Control.Monad.ST                   as ST
import qualified Data.Vector.Fusion.Stream.Monadic  as S


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

asList
    :: forall s a.
       MQueue s a
    -> ST.ST s [a]
asList (MQueue deque) =
    go []
  where
    go :: [a] -> ST.ST s [a]
    go lst = do
        itemM <- Mut.popFront deque
        maybe (return lst) (\item -> go (item : lst)) itemM

-- | Convert into 'Stream' (starting from the front)
mstream :: (PrimMonad m)
        => MQueue (PrimState m) a
        -> m (S.Stream m a)
mstream = Mut.mstream . getDeque

-- | Convert into 'Stream' (starting from the back)
mstreamR :: (PrimMonad m)
        => MQueue (PrimState m) a
        -> m (S.Stream m a)
mstreamR = Mut.mstreamR . getDeque
