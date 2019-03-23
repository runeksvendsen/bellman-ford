{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module BellmanFord.Types
( TestEdge
, PositiveWeight(..)
)
where

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

newtype PositiveWeight a = PositiveWeight { positiveWeight :: a }
   deriving (Eq, Show, Ord)

instance Monad m => SS.Serial m (PositiveWeight TestEdge) where
   series = do
      SS.Positive weight' <- SS.series
      edge <- SS.series
      return $ PositiveWeight $ TestEdge edge weight'
