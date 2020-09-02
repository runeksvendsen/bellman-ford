module Util
( -- * Module functions
  sameUniqueSequenceAs
, fromIdxEdge
  -- * Test functions
, test_sameUniqueSequenceAs
)
where

import qualified Data.Graph.Digraph                 as Lib
import           Types.Edge

import qualified Data.List

fromIdxEdge
    :: (Lib.HasWeight meta Double)
    => Lib.IdxEdge String meta -> TestEdge
fromIdxEdge idxEdge = TestEdge (Lib.fromNode idxEdge) (Lib.toNode idxEdge) (Lib.weight $ Lib.metaData idxEdge)

-- | Are the two given sequence "the same"?
--   Here, two sequences "A" and "B" are defined as "the same"
--    if it's possible to split sequence "A"
--    in two at a single point, called A1 and A2,
--    such that A2++A1 == B.
--  Example:
--   "[4,5,6,1,2,3] `sameUniqueSequenceAs` [1,2,3,4,5,6] = True"
--     because "[1,2,3]++[4,5,6] = [1,2,3,4,5,6]""
--  Requirement:
--   * a sequence must contain only unique items.
--      ie.: for each sequence 's': "nub s == s"
sameUniqueSequenceAs
    :: Eq a
    => [a]  -- ^ Sequence to test
    -> [a]  -- ^ Expected sequence
    -> Bool
sameUniqueSequenceAs [] []    = True
sameUniqueSequenceAs (_:_) [] = False
sameUniqueSequenceAs testLst expected@(expHead : _) =
    Data.List.dropWhile isNotExpectedHead testLst
        ++ Data.List.takeWhile isNotExpectedHead testLst
            == expected
  where
    isNotExpectedHead = (/= expHead)

test_sameUniqueSequenceAs
    :: Eq a
    => [a]  -- ^ An arbitrary list containing unique elements ("nub list == list")
    -> a    -- ^ Some element that is present in the arbitrary list
    -> Bool
test_sameUniqueSequenceAs arbList arbListItem =
    splitList `sameUniqueSequenceAs` arbList
  where
    isNotSplitElement = (/= arbListItem)
    splitList =
        Data.List.dropWhile isNotSplitElement arbList
            ++ Data.List.takeWhile isNotSplitElement arbList
