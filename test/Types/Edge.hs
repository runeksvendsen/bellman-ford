{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Types.Edge
( TestEdge(..)
, PositiveWeight(..)
, NegLog(..)
)
where

import           Data.Graph.Digraph                   as Lib
import           Data.Graph.Edge                      as Lib
import qualified Test.SmallCheck.Series               as SS


data TestEdge = TestEdge
    { getFrom     :: String
    , getTo       :: String
    , getWeight   :: Double
    } deriving (Eq, Show, Ord)

instance Lib.DirectedEdge TestEdge String where
   fromNode = getFrom
   toNode = getTo

instance Lib.WeightedEdge TestEdge String Double where
   weight = getWeight

instance Monad m => SS.Serial m TestEdge where
   series = TestEdge <$> SS.series <*> SS.series <*> SS.series

newtype PositiveWeight a = PositiveWeight { positiveWeight :: a }
   deriving (Eq, Show, Ord)

instance Monad m => SS.Serial m (PositiveWeight TestEdge) where
   series = do
      SS.Positive weight' <- SS.series
      edge <- SS.series
      return $ PositiveWeight $ edge { getWeight = weight' }

-- | The negative log of something
newtype NegLog a = NegLog { getLog :: a }
   deriving (Eq, Show, Ord)

-- | Same instance as for 'TestEdge'
instance Lib.DirectedEdge (NegLog TestEdge) String where
   fromNode = fromNode . getLog
   toNode = toNode . getLog

-- | Return negative log of weight
instance Lib.WeightedEdge (NegLog TestEdge) String Double where
   weight = negate . log . weight . getLog
