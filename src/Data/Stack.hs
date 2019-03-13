{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Stack
( -- * Data types
  MStack
  -- * Construction
, new
, singleton
  -- * Modification
, push
, pop
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


-- | Mutable stack
newtype MStack s a = MStack { getStack :: Mut.Deque Vec.MVector s a }

new :: (PrimMonad m)
    => m (MStack (PrimState m) a)
new = MStack <$> Mut.newColl

singleton
    :: (PrimMonad m)
    => a
    -> m (MStack (PrimState m) a)
singleton item = do
    stack <- new
    push stack item
    return stack

push
    :: (PrimMonad m)
    => MStack (PrimState m) a
    -> a
    -> m ()
push (MStack deque) = Mut.pushFront deque

pop
    :: (PrimMonad m)
    => MStack (PrimState m) a
    -> m (Maybe a)
pop = Mut.popFront . getStack

-- | Convert into 'Stream' (starting from the front)
mstream :: (PrimMonad m)
        => MStack (PrimState m) a
        -> m (S.Stream m a)
mstream = Mut.mstream . getStack

-- | Convert into 'Stream' (starting from the back)
mstreamR :: (PrimMonad m)
        => MStack (PrimState m) a
        -> m (S.Stream m a)
mstreamR = Mut.mstreamR . getStack
