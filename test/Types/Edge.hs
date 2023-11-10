{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types.Edge
( TestEdge(..), idxEdgeToTestEdge
, NonNegativeWeight(..)
, BoundedIntegral, getBoundedIntegral
, FuzzyOrd(..)
)
where

import           Data.Graph.Digraph                   as Lib
import qualified Test.SmallCheck.Series               as SS
import qualified Test.Tasty.QuickCheck                as QC
import qualified Data.List.NonEmpty as NE
import qualified Data.Graph.SP.Double
import Data.Int

data TestEdge weight = TestEdge
    { getFrom     :: String
    , getTo       :: String
    , getWeight   :: weight
    } deriving (Eq, Show, Ord)

instance Functor TestEdge where
   fmap f e = e{ getWeight = f (getWeight e) }

instance Lib.DirectedEdge (TestEdge weight) String weight where
   fromNode = getFrom
   toNode = getTo
   metaData = getWeight

idxEdgeToTestEdge
   :: Lib.IdxEdge String weight
   -> TestEdge weight
idxEdgeToTestEdge idx =
   TestEdge (Lib.eFrom idx) (Lib.eTo idx) (Lib.eMeta idx)

instance (Monad m, SS.Serial m weight) => SS.Serial m (TestEdge weight) where
   series = TestEdge <$> SS.series <*> SS.series <*> SS.series

instance QC.Arbitrary weight => QC.Arbitrary (TestEdge weight) where
   arbitrary = TestEdge <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

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

newtype BoundedIntegral bound int = BoundedIntegral { getBoundedIntegral :: int }
   deriving (Eq, Ord, Functor, Num, Bounded)

instance Integral int => FuzzyOrd (BoundedIntegral bound int) where
   fuzzyLT = (<)

instance Show int => Show (BoundedIntegral bound int) where
   show = show . getBoundedIntegral

instance (Monad m, Num int, SS.Serial m bound, Integral bound)
   => SS.Serial m (BoundedIntegral bound int) where
      series = do
         weight' :: bound <- SS.series
         pure $ BoundedIntegral (fromIntegral weight')

instance (Num int, QC.Arbitrary bound, Integral bound)
   => QC.Arbitrary (BoundedIntegral bound int) where
      arbitrary = do
         weight' :: bound <- QC.arbitrary
         pure $ BoundedIntegral (fromIntegral weight')

-- Orphan
instance (QC.Arbitrary a) => QC.Arbitrary (NE.NonEmpty a) where
   arbitrary = fmap NE.fromList $
      QC.arbitrary `QC.suchThat` (not . null)

-- | Due to floating point rounding errors, using @< 0@ as the constraint on a negative cycle is not enough.
--   For 'Double', we need to instead use 'Data.Graph.SP.Double.isLessThan'.
--   This class exists only to support using a different "less than" function for 'Double'.
class Eq a => FuzzyOrd a where
   fuzzyLT :: a -> a -> Bool

instance FuzzyOrd Double where
   fuzzyLT = Data.Graph.SP.Double.isLessThan

instance FuzzyOrd Int64 where
   fuzzyLT = (<)
