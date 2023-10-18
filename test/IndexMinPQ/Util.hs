{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
module IndexMinPQ.Util
( Positive(..)
)
where

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
