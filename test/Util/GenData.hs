{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Util.GenData
( ArbData(..)
, GenData(..)
, SuchThat(..)
)
where

import qualified Test.SmallCheck.Series               as SS
import qualified Test.Tasty.QuickCheck                as QC
import           Control.Applicative                  (empty)


-- | Monads in which test data can be generated
class Monad m => ArbData m where
    getListSize :: m Int -- ^ The length to use when generating a list

-- | Generate test data á la 'QC.arbitrary' and 'SS.series'
class SuchThat m => GenData m a where
    genData :: m a

-- | Generate test data that satisfies a predicate
class ArbData m => SuchThat m where
    suchThat :: m a -> (a -> Bool) -> m a

-- SmallCheck
instance ArbData (SS.Series m) where
    getListSize = SS.getDepth
instance SS.Serial m a => GenData (SS.Series m) a where
    genData = SS.series
instance SuchThat (SS.Series m) where
    suchThat s p = s >>= \x -> if p x then pure x else empty

-- QuickCheck
instance ArbData QC.Gen where
    getListSize = do
        n <- QC.getSize
        QC.choose(0, n)
instance QC.Arbitrary a => GenData QC.Gen a where
    genData = QC.arbitrary
instance SuchThat QC.Gen where
    suchThat = QC.suchThat
