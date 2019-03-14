{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module BellmanFord.Types where

import           BellmanFord.Orphans                         ()
import           Data.Graph.Digraph                   as Lib
import           Data.Graph.Edge                      as Lib
import qualified Test.SmallCheck.Series               as SS


data TestEdge = TestEdge
    { getEdge     :: (String, String)
    , getWeight   :: Double
    } deriving (Eq, Show)

instance Lib.DirectedEdge TestEdge String where
   fromNode = fst . getEdge
   toNode = snd . getEdge

instance Lib.WeightedEdge TestEdge String Double where
   weight = getWeight

instance Monad m => SS.Serial m TestEdge where
   series = TestEdge <$> SS.series <*> SS.series
