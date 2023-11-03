{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Types.Edge
( TestEdge(..)
, NonNegativeWeight(..)
, BoundedIntegral(..)
)
where

import           Data.Graph.Digraph                   as Lib
import qualified Test.SmallCheck.Series               as SS
import qualified Test.Tasty.QuickCheck                as QC
import Data.Proxy (Proxy (Proxy))

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
   deriving (Eq, Ord, Functor)

instance (Bounded bound, Show int, Integral bound, Integral int) => Bounded (BoundedIntegral bound int) where
   maxBound = fromIntegral (maxBound :: bound)
   minBound = fromIntegral (minBound :: bound)

instance (Bounded bound, Num bound, Show int, Integral int, Integral bound) => Num (BoundedIntegral bound int) where
   BoundedIntegral a + BoundedIntegral b =
      BoundedIntegral $ checkBounds (Proxy :: Proxy bound) $ a + b
   BoundedIntegral a * BoundedIntegral b =
      BoundedIntegral $ checkBounds (Proxy :: Proxy bound) $ a * b
   abs =
      BoundedIntegral . abs . getBoundedIntegral
   signum =
      BoundedIntegral . signum . getBoundedIntegral
   fromInteger =
      BoundedIntegral . checkBounds (Proxy :: Proxy bound) . fromInteger
   negate =
      BoundedIntegral . checkBounds (Proxy :: Proxy bound) . negate . getBoundedIntegral

checkBounds
   :: forall int bound.
      ( Show int
      , Bounded bound
      , Integral bound
      , Num int
      , Ord int
      )
   => Proxy bound
   -> int
   -> int
checkBounds _ i
   | i > (fromIntegral (maxBound :: bound)) = error $ "BoundedIntegral: overflow: " <> show i
   | i < (fromIntegral (minBound :: bound)) = error $ "BoundedIntegral: underflow: " <> show i
   | otherwise = i

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
