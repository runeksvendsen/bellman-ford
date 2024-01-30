{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
module IndexMinPQ.Util
( Positive(..)
, ListWithIndex(..)
)
where

import Util.GenData
import qualified Test.QuickCheck as QC
import qualified Test.SmallCheck.Series as SS

-- | a 'Positive' for both QuickCheck and SmallCheck
newtype Positive a = Positive { unPositive :: a }
   deriving (Eq, Ord, Functor, Enum)

instance Show a => Show (Positive a) where
   show = show . unPositive

instance (SS.Serial m a, Ord a, Num a, Monad m) => SS.Serial m (Positive a) where
   series =
      Positive . SS.getPositive <$> SS.series

instance (QC.Arbitrary a, Ord a, Num a) => QC.Arbitrary (Positive a) where
   arbitrary =
      Positive . QC.getPositive <$> QC.arbitrary

-- | A list along with an integer @i@ where @0 <= i <= length list@
data ListWithIndex a = ListWithIndex [a] Int
   deriving (Eq, Show)

instance Functor ListWithIndex where
   fmap f (ListWithIndex list i) = ListWithIndex (map f list) i

instance (SS.Serial m a, Monad m) => SS.Serial m (ListWithIndex a) where
   series = do
      list <- SS.series
      i <- SS.series `suchThat` \i -> 0 <= i && i <= length list
      pure $ ListWithIndex list i

instance (QC.Arbitrary a) => QC.Arbitrary (ListWithIndex a) where
   arbitrary = do
      list <- QC.arbitrary
      i <- QC.chooseInt (0, length list)
      pure $ ListWithIndex list i
