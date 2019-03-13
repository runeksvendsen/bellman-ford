{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Spec.Orphans where

import           Data.Graph.Digraph                   as Lib
import qualified Test.SmallCheck.Series               as SS
import           Control.Monad.Primitive              (PrimMonad(..))
import           Control.Monad.Trans.Class            (MonadTrans(..))
import           Data.Functor.Identity                (Identity)


instance PrimMonad m => PrimMonad (SS.Series m) where
  type PrimState (SS.Series m) = PrimState m
  primitive = lift . primitive
  {-# INLINE primitive #-}

instance (PrimMonad m, PrimState m ~ primState, DirectedEdge e v, SS.Serial Identity e) 
   => SS.Serial m (Lib.Digraph primState g e v) where
      series = do
         depth <- SS.getDepth
         let edgeList = SS.listSeries depth
         Lib.fromEdges edgeList
