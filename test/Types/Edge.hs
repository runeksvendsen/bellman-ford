{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import qualified Data.Text                            as T
import qualified Control.Monad.Random.Class           as Random
import qualified Control.Monad.Trans.Class            as Trans
import           GHC.Word (Word64)


data TestEdge = TestEdge
    { getFrom     :: String
    , getTo       :: String
    , getWeight   :: Double
    , getMultiKey :: Text
    } deriving (Eq, Show, Ord)

instance Lib.DirectedEdge TestEdge String where
   fromNode = getFrom
   toNode = getTo
   multiKey = getMultiKey

instance Lib.WeightedEdge TestEdge String Double where
   weight = getWeight

instance SS.Serial IO TestEdge where
   series =
      let rndNumM = T.pack . show <$> Random.getRandomR (minBound :: Word64, maxBound)
      in TestEdge <$> SS.series <*> SS.series <*> SS.series <*> rndNumM

instance QC.Arbitrary TestEdge where
   arbitrary = TestEdge <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

newtype Unweighted a = Unweighted { unweighted :: a }
   deriving (Eq, Show, Ord)

instance SS.Serial IO (Unweighted TestEdge) where
   series =
      let rndNumM = T.pack . show <$> Random.getRandomR (minBound :: Word64, maxBound)
      in Unweighted <$> (TestEdge <$> SS.series <*> SS.series <*> return 0.0 <*> rndNumM)

instance QC.Arbitrary (Unweighted TestEdge) where
   arbitrary = Unweighted <$> (TestEdge <$> QC.arbitrary <*> QC.arbitrary <*> return 0.0 <*> QC.arbitrary)

newtype PositiveWeight = PositiveWeight { positiveWeight :: TestEdge }
   deriving (Eq, Ord)

instance Show PositiveWeight where
   show = show . positiveWeight

instance SS.Serial IO PositiveWeight where
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
                     <*> QC.arbitrary
      in PositiveWeight <$> positiveEdge

-- | The negative log of something
newtype NegLog a = NegLog { getLog :: a }
   deriving (Eq, Show, Ord)

-- | Same instance as for 'TestEdge'
instance Lib.DirectedEdge (NegLog TestEdge) String where
   fromNode = fromNode . getLog
   toNode = toNode . getLog
   multiKey = multiKey . getLog

-- | Return negative log of weight
instance Lib.WeightedEdge (NegLog TestEdge) String Double where
   weight = negate . log . weight . getLog

instance Monad m => SS.Serial m Text where
   series = T.pack <$> SS.series

instance QC.Arbitrary Text where
   arbitrary = T.pack <$> QC.arbitrary

instance Random.MonadRandom (SS.Series IO) where
   getRandomR = Trans.lift . Random.getRandomR
   getRandom = Trans.lift Random.getRandom
   getRandomRs = Trans.lift . Random.getRandomRs
   getRandoms = Trans.lift Random.getRandoms
