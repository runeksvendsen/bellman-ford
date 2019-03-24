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
, toList
)
where

import           Data.Mutable                       (PrimMonad, PrimState)
import qualified Data.Mutable                       as Mut
import qualified Data.Vector                        as Vec


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

-- | Convert into list
toList :: (PrimMonad m)
        => MStack (PrimState m) a
        -> m [a]
toList = Mut.toList . getStack
