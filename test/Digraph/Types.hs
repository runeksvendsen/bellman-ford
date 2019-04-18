{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
module Digraph.Types
( ModifyGraph(..)
, modify
)
where

import           Data.Graph.Digraph                   as Lib
import qualified Test.SmallCheck.Series               as SS
import           GHC.Generics                         (Generic)
import           Control.Monad.Primitive              (PrimMonad, PrimState)
import qualified System.Random.Shuffle                as Shuffle
import qualified Control.Monad.Random.Class           as Random
import           Control.Monad.Trans.Class            (lift)


-- | Various ways to modify a graph
data ModifyGraph e
    = InsertEdge e
    | RemoveEdge e
        deriving (Eq, Show, Functor, Generic)

instance (SS.Serial m e, Random.MonadRandom m) => SS.Serial m [ModifyGraph e] where
    series = do
        edges <- SS.series
        lift $ Shuffle.shuffleM $ fmap InsertEdge edges ++ fmap RemoveEdge edges

modify
    :: (PrimMonad m, Lib.DirectedEdge e v)
    => Lib.Digraph (PrimState m) g e v
    -> ModifyGraph e
    -> m ()
modify g (InsertEdge e) = Lib.insertEdge g e
modify g (RemoveEdge e) = Lib.removeEdge g e
