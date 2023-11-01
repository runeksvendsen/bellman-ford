{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Types.Edge
( TestEdge(..)
, PositiveWeight(..)
, NonNegativeWeight(..)
, NegLog(..)
)
where

import           Data.Graph.Digraph                   as Lib
import qualified Test.SmallCheck.Series               as SS
import qualified Test.Tasty.QuickCheck                as QC


data TestEdge weight = TestEdge
    { getFrom     :: String
    , getTo       :: String
    , getWeight   :: weight
    } deriving (Eq, Show, Ord)

instance Lib.DirectedEdge (TestEdge weight) String weight where
   fromNode = getFrom
   toNode = getTo
   metaData = getWeight

instance (Monad m, SS.Serial m weight) => SS.Serial m (TestEdge weight) where
   series = TestEdge <$> SS.series <*> SS.series <*> SS.series

instance QC.Arbitrary weight => QC.Arbitrary (TestEdge weight) where
   arbitrary = TestEdge <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

newtype PositiveWeight weight = PositiveWeight { positiveWeight :: TestEdge weight }
   deriving (Eq, Ord)

instance Show weight => Show (PositiveWeight weight) where
   show = show . positiveWeight

instance (Monad m, Num weight, SS.Serial m weight, Num weight, Ord weight) => SS.Serial m (PositiveWeight weight) where
   series = do
      SS.Positive weight' <- SS.series
      edge :: TestEdge weight <- SS.series
      return $ PositiveWeight $ edge { getWeight = weight' }

instance (Num weight, Ord weight, QC.Arbitrary weight) => QC.Arbitrary (PositiveWeight weight) where
   arbitrary =
      let positiveEdge =
            TestEdge <$> QC.arbitrary
                     <*> QC.arbitrary
                     <*> fmap QC.getPositive QC.arbitrary
      in PositiveWeight <$> positiveEdge

newtype NonNegativeWeight weight = NonNegativeWeight { nonNegativeWeight :: TestEdge weight }
   deriving (Eq, Ord)

instance Show weight => Show (NonNegativeWeight weight) where
   show = show . nonNegativeWeight

instance (Monad m, Num weight, Ord weight, SS.Serial m weight) => SS.Serial m (NonNegativeWeight weight) where
   series = do
      SS.NonNegative weight' <- SS.series
      edge :: TestEdge weight <- SS.series
      return $ NonNegativeWeight $ edge { getWeight = weight' }

instance (Num weight, Ord weight, QC.Arbitrary weight) => QC.Arbitrary (NonNegativeWeight weight) where
   arbitrary =
      let nonNegativeEdge =
            TestEdge <$> QC.arbitrary
                     <*> QC.arbitrary
                     <*> fmap QC.getNonNegative QC.arbitrary
      in NonNegativeWeight <$> nonNegativeEdge

-- | The negative log of something
newtype NegLog a = NegLog { getLog :: a }
   deriving (Eq, Show, Ord)

-- | Same instance as for 'TestEdge'
instance Lib.DirectedEdge (NegLog (TestEdge weight)) String weight where
   fromNode = fromNode . getLog
   toNode = toNode . getLog
   metaData = metaData . getLog
