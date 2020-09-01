{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Types.Edge
( TestEdge(..)
, PositiveWeight(..)
, Unweighted(..)
, NegLog(..)
)
where

import           Data.Graph.Digraph                   as Lib
import           Data.Graph.Edge                      as Lib
import qualified Test.SmallCheck.Series               as SS
import qualified Test.Tasty.QuickCheck                as QC


data TestEdge = TestEdge
    { getFrom     :: String
    , getTo       :: String
    , getWeight   :: Double
    } deriving (Eq, Show, Ord)

instance Lib.DirectedEdge TestEdge String Double where
   fromNode = getFrom
   toNode = getTo
   metaData = getWeight

instance Lib.HasWeight Double Double where
   weight = id

instance Monad m => SS.Serial m TestEdge where
   series = TestEdge <$> SS.series <*> SS.series <*> SS.series

instance QC.Arbitrary TestEdge where
   arbitrary = TestEdge <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

newtype Unweighted a = Unweighted { unweighted :: a }
   deriving (Eq, Show, Ord)

instance Monad m => SS.Serial m (Unweighted TestEdge) where
   series =
      Unweighted <$> (TestEdge <$> SS.series <*> SS.series <*> return 0.0)

instance QC.Arbitrary (Unweighted TestEdge) where
   arbitrary = Unweighted <$> (TestEdge <$> QC.arbitrary <*> QC.arbitrary <*> return 0.0)

newtype PositiveWeight = PositiveWeight { positiveWeight :: TestEdge }
   deriving (Eq, Ord)

instance Show PositiveWeight where
   show = show . positiveWeight

instance Monad m => SS.Serial m PositiveWeight where
   series = do
      SS.Positive weight' <- SS.series
      edge <- SS.series
      return $ PositiveWeight $ edge { getWeight = weight' }

instance QC.Arbitrary PositiveWeight where
   arbitrary =
      let positiveEdge =
            TestEdge <$> QC.arbitrary
                     <*> QC.arbitrary
                     <*> fmap QC.getPositive QC.arbitrary
      in PositiveWeight <$> positiveEdge

-- | The negative log of something
newtype NegLog a = NegLog { getLog :: a }
   deriving (Eq, Show, Ord)

-- | Same instance as for 'TestEdge'
instance Lib.DirectedEdge (NegLog TestEdge) String Double where
   fromNode = fromNode . getLog
   toNode = toNode . getLog
   metaData = metaData . getLog
