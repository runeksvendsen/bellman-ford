{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Util.GenData
( GenData(..)
, SuchThat(..)
)
where

import qualified Test.SmallCheck.Series               as SS
import qualified Test.Tasty.QuickCheck                as QC
import           Control.Applicative                  (empty)


-- | Generate test data á la 'QC.arbitrary' and 'SS.series'
class SuchThat m => GenData m a where
    genData :: m a

-- | Generate test data that satisfies a predicate
class Monad m => SuchThat m where
    suchThat :: m a -> (a -> Bool) -> m a

-- SmallCheck
instance SS.Serial m a => GenData (SS.Series m) a where
    genData = SS.series
instance SuchThat (SS.Series m) where
    suchThat s p = s >>= \x -> if p x then pure x else empty

-- QuickCheck
instance QC.Arbitrary a => GenData (QC.Gen) a where
    genData = QC.arbitrary
instance SuchThat (QC.Gen) where
    suchThat = QC.suchThat
