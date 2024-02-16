module Data.Graph.Prelude
( module Export
, whenM
, unlessM
, ifM
) where

import Control.Monad                as Export
import Data.Maybe                   as Export
import Data.Hashable                as Export (Hashable)
import Control.Monad.Primitive      as Export (PrimMonad, PrimState)
import Text.Printf                  as Export
import Control.Exception            as Export (assert)
import Control.Monad.ST             as Export

{-# INLINE whenM #-}
whenM :: Monad m => m Bool -> m () -> m ()
whenM p m =
  p >>= flip when m

{-# INLINE unlessM #-}
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p m =
  p >>= flip unless m

{-# INLINE ifM #-}
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p x y = p >>= \b -> if b then x else y
