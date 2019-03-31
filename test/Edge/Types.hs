{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Edge.Types
( TestEdge(..)
, PositiveWeight(..)
, NegLog(..)
, NegativeCycle(..)
)
where

import           Data.Graph.Digraph                   as Lib
import           Data.Graph.Edge                      as Lib
import qualified Test.SmallCheck.Series               as SS
import qualified Data.List.NonEmpty                   as NE
import           Data.Graph.Cycle                     (verifyCycle)
import           Data.Maybe                           (isNothing)
import qualified Control.Exception                    as Ex
import           Control.Applicative                  (empty)

import Debug.Trace


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
   series = "lol" `trace` TestEdge <$> SS.series <*> SS.series <*> SS.series

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

newtype EdgeCycle = EdgeCycle { getEdgeCycle :: NE.NonEmpty TestEdge }
   deriving (Eq, Show, Ord)

instance Monad m => SS.Serial m EdgeCycle where
   series = do
      randomEdges <- NE.fromList . SS.getNonEmpty <$> SS.series
      let firstEdge = (NE.head randomEdges) { getFrom = getTo $ NE.last randomEdges}
          edgeList = foldr folder (firstEdge NE.:| []) (NE.tail randomEdges)
          isValidCycle = isNothing . verifyCycle $ NE.toList edgeList
      return $ EdgeCycle $ Ex.assert isValidCycle edgeList
    where
      folder nextEdge accum@(prevEdge NE.:| _) =
         joinEdges prevEdge nextEdge `NE.cons` accum
      joinEdges (TestEdge _ toA _) edgeB@(TestEdge _ toB _) =
         edgeB { getFrom = toA, getTo = toB }

suchThat :: SS.Series m a -> (a -> Bool) -> SS.Series m a
suchThat s p = s >>= \x -> if p x then pure x else empty

newtype NegativeCycle = NegativeCycle { getNegativeCycle :: NE.NonEmpty TestEdge }
   deriving (Eq, Show, Ord)

instance Monad m => SS.Serial m NegativeCycle where
   series = do
      let negativeWeightSum edgeCycle =
            sum (NE.map weight $ getEdgeCycle edgeCycle) < 0
      NegativeCycle . getEdgeCycle <$> SS.series `suchThat` negativeWeightSum
