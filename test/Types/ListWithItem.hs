{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Types.ListWithItem
( ListWithItem(..)
, genUniqueListWithItem
)
where

import qualified Data.List.NonEmpty                 as NE
import           Util.GenData                       (GenData(..), suchThat)
import qualified Test.SmallCheck.Series             as SS
import qualified Test.Tasty.QuickCheck              as QC


-- | Some list along with an item present in that list
data ListWithItem a = ListWithItem
    (NE.NonEmpty a) -- ^ A list
    a               -- ^ An item present in the list
        deriving (Eq, Show)

-- | Given a non-empty list, return all possible variations of
--    'ListWithItem' for the given list.
allListWithItem
    :: NE.NonEmpty a
    -> NE.NonEmpty (ListWithItem a)
allListWithItem ne =
    NE.map (ListWithItem ne) ne

genUniqueListWithItem
    :: (GenData m [a], Eq a)
    => m (NE.NonEmpty (ListWithItem a))
genUniqueListWithItem =
    allListWithItem <$> uniqueNonEmptyList
  where
    uniqueNonEmptyList = nonEmptyList `suchThat` (\lst -> NE.nub lst == lst)
    nonEmptyList = NE.fromList <$> (genData `suchThat` ((> 0) . length))

instance (SS.Serial m a, Eq a) => SS.Serial m (ListWithItem a) where
    series = do
        uniqueListWithItems <- genUniqueListWithItem
        SS.generate (const $ NE.toList uniqueListWithItems)

instance (QC.Arbitrary a, Eq a) => QC.Arbitrary (ListWithItem a) where
    arbitrary = do
        uniqueListWithItems <- genUniqueListWithItem
        QC.elements $ NE.toList uniqueListWithItems
