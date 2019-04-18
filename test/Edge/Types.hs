{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Edge.Types
( TestEdge(..)
, Unweighted(..)
, PositiveWeight(..)
, NegLog(..)
)
where

import           Data.Graph.Digraph                   as Lib
import           Data.Graph.Edge                      as Lib
import qualified Test.SmallCheck.Series               as SS


data TestEdge = TestEdge
    { getEdge     :: (String, String)
    , getWeight   :: Double
    } deriving (Eq, Show, Ord)

instance Lib.DirectedEdge TestEdge String where
   fromNode = fst . getEdge
   toNode = snd . getEdge

instance Lib.WeightedEdge TestEdge String Double where
   weight = getWeight

instance Monad m => SS.Serial m TestEdge where
   series = TestEdge <$> SS.series <*> SS.series

newtype Unweighted a = Unweighted { unweighted :: a }
   deriving (Eq, Show, Ord)

instance Monad m => SS.Serial m (Unweighted TestEdge) where
   series =
      Unweighted <$> (TestEdge <$> SS.series <*> return 0.0)

newtype PositiveWeight a = PositiveWeight { positiveWeight :: a }
   deriving (Eq, Show, Ord)

instance Monad m => SS.Serial m (PositiveWeight TestEdge) where
   series = do
      SS.Positive weight' <- SS.series
      edge <- SS.series
      return $ PositiveWeight $ TestEdge edge weight'

-- | The negative log of something
newtype NegLog a = NegLog { getLog :: a }
   deriving (Eq, Show, Ord)

-- | Same instance as for 'TestEdge'
instance Lib.DirectedEdge (NegLog TestEdge) String where
   fromNode = fst . getEdge . getLog
   toNode = snd . getEdge . getLog

-- | Return negative log of weight
instance Lib.WeightedEdge (NegLog TestEdge) String Double where
   weight = negate . log . getWeight . getLog
