module Data.Graph.Util
( nubOrd
) where

import Data.List (group, sort)

-- | Faster version of 'Data.List.nub'.
--   O(n log n) instead of O(n^2), but requires 'Ord' instance.
nubOrd :: Ord a => [a] -> [a]
nubOrd = map head . group . sort
