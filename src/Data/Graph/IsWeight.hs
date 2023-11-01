{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Graph.IsWeight
( IsWeight(..)
, Boxed(..), Unboxed(..)
)
where

import qualified Data.Array.ST as Arr
import qualified Data.Array.ST as ST
import Control.Monad.ST

newtype Boxed a = Boxed { getBoxed :: a }
    deriving (Eq, Read, Show, Ord, Functor)
newtype Unboxed a = Unboxed { getUnboxed :: a }
    deriving (Eq, Read, Show, Ord, Functor)

-- | A weight that can be used with the shortest path algorithms implemented by this library.
--
--   This is necessary to allow both:
--      (a) arbitrary edge weights
--      (b) better performance for unboxed edge weights (e.g. 'Int', 'Double')
class IsWeight weight s where
    type Array weight s

    newArray :: (Int, Int) -> weight -> ST s (Array weight s)
    getBounds :: Array weight s -> ST s (Int, Int)
    writeArray :: Array weight s -> Int -> weight -> ST s ()
    readArray :: Array weight s -> Int -> ST s weight

instance Arr.MArray (Arr.STUArray s) weight (ST s) => IsWeight (Unboxed weight) s where
    type Array (Unboxed weight) s = ST.STUArray s Int weight
    newArray range (Unboxed weight) = ST.newArray range weight
    getBounds = ST.getBounds
    writeArray array idx (Unboxed weight) = ST.writeArray array idx weight
    readArray array idx  = Unboxed <$> ST.readArray array idx

instance IsWeight (Boxed weight) s where
    type Array (Boxed weight) s = ST.STArray s Int weight
    newArray range (Boxed weight) = ST.newArray range weight
    getBounds = ST.getBounds
    writeArray array idx (Boxed weight) = ST.writeArray array idx weight
    readArray array idx  = Boxed <$> ST.readArray array idx
