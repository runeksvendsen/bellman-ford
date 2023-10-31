{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Graph.IsWeight
( IsWeight(..)
, Boxed, Unboxed
)
where

import qualified Data.Array.ST as Arr
import qualified Control.Monad.ST as Strict
import qualified Control.Monad.ST.Lazy as Lazy
import Data.Kind (Type)
import Data.Proxy (Proxy)
import qualified Data.Array.ST as ST

data Boxed a
data Unboxed a

-- | A weight that can be used with the shortest path algorithms defined in this library.
--
--   This is necessary to allow both:
--      (a) arbitrary edge weights
--      (b) better performance for unboxed edge weights (e.g. 'Int', 'Double')
class IsWeight weight s (m :: Type -> Type) where
    type Array weight

    newArray :: (Int, Int) -> weight -> m (Array weight)
    getBounds :: Proxy weight -> Array weight -> m (Int, Int)
    writeArray :: Array weight -> Int -> weight -> m ()
    readArray :: Proxy weight -> Array weight -> Int -> m a

instance Arr.MArray (Arr.STUArray s) weight (Strict.ST s) => IsWeight (Unboxed weight) s (Strict.ST s) where
    type Array weight = ST.STUArray s Int weight

    newArray = ST.newArray

    getBounds _ = ST.getBounds
    writeArray = ST.writeArray
    readArray _ = ST.readArray

instance IsWeight (Boxed weight) s (Strict.ST s)
instance IsWeight (Boxed weight) s (Lazy.ST s)
