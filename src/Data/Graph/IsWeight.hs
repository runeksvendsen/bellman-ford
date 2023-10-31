{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Graph.IsWeight
( IsWeight(..)
, Boxed(..), Unboxed(..)
)
where

import qualified Data.Array.ST as Arr
import qualified Control.Monad.ST as Strict
import qualified Control.Monad.ST.Lazy as Lazy
import qualified Data.Array.ST as ST

newtype Boxed a = Boxed { getBoxed :: a }
newtype Unboxed a = Unboxed { getUnboxed :: a }

-- | A weight that can be used with the shortest path algorithms implemented by this library.
--
--   This is necessary to allow both:
--      (a) arbitrary edge weights
--      (b) better performance for unboxed edge weights (e.g. 'Int', 'Double')
class IsWeight weight s m | m -> s where
    type Array weight s

    newArray :: (Int, Int) -> weight -> m (Array weight s)
    getBounds :: Array weight s -> m (Int, Int)
    writeArray :: Array weight s -> Int -> weight -> m ()
    readArray :: Array weight s -> Int -> m weight

instance Arr.MArray (Arr.STUArray s) weight (Strict.ST s) => IsWeight (Unboxed weight) s (Strict.ST s) where
    type Array (Unboxed weight) s = ST.STUArray s Int weight
    newArray range (Unboxed weight) = ST.newArray range weight
    getBounds = ST.getBounds
    writeArray array idx (Unboxed weight) = ST.writeArray array idx weight
    readArray array idx  = Unboxed <$> ST.readArray array idx

instance IsWeight (Boxed weight) s (Strict.ST s) where
    type Array (Boxed weight) s = ST.STArray s Int weight
    newArray range (Boxed weight) = ST.newArray range weight
    getBounds = ST.getBounds
    writeArray array idx (Boxed weight) = ST.writeArray array idx weight
    readArray array idx  = Boxed <$> ST.readArray array idx

instance IsWeight (Boxed weight) s (Lazy.ST s) where
    type Array (Boxed weight) s = ST.STArray s Int weight
    newArray range (Boxed weight) = ST.newArray range weight
    getBounds = ST.getBounds
    writeArray array idx (Boxed weight) = ST.writeArray array idx weight
    readArray array idx  = Boxed <$> ST.readArray array idx
