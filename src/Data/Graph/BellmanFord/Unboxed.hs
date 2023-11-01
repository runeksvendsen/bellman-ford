{-# LANGUAGE ConstraintKinds #-}
-- | "Data.Graph.BellmanFord" specialized to work with all unboxed edge weights.
--
--   Where "unboxed" refers to anything that can be stored in an unboxed array
--    (e.g. 'Data.Array.ST.STUArray' and 'Data.Array.IO.IOUArray').
module Data.Graph.BellmanFord.Unboxed
( -- * Monad
  runBF
, BF
, Unboxable
  -- * Algorithm
, bellmanFord
  -- * Queries
, pathTo
, negativeCycle
  -- * Extras
, BF.getGraph
  -- * Re-exports
, E.DirectedEdge(..)
)
where

import Prelude
import qualified Data.Graph.BellmanFord as BF
import           Prelude                            hiding (cycle)
import           Data.Graph.Prelude
import           Data.Graph.IsWeight
import qualified Data.Graph.Digraph                 as DG
import qualified Data.Graph.Edge                    as E
import qualified Data.List.NonEmpty                 as NE
import qualified Data.Array.ST as Arr

type BF s v weight meta = BF.BF s v (Unboxed weight) meta

type Unboxable weight s = Arr.MArray (Arr.STUArray s) weight (ST s)

-- |
runBF
    :: Unboxable weight s
    => DG.Digraph s v meta
    -> (weight -> meta -> weight)
    -- ^ Weight combination function @f@.
    --   @f a b@ calculates a new distance to a /to/-vertex.
    --   @a@ is the distance to the edge's /from/-vertex,
    --    and @b@ is the edge going from the /from/-vertex to the /to/-vertex.
    --   If the value returned by this
    --    function is less than the current distance to /to/ the distance to /to/ will
    --    be updated.
    --  E.g. for Dijkstra with type parameter @e@ equal to 'Double',
    --   this function would simply be @('+')@.
    -> (weight -> weight -> Bool)
    -- ^ @isLessThan@ function. @isLessThan a b@ should return 'True' if @a@ is strictly less than @b@, and 'False' otherwise.
    -> weight
    -- ^ "Zero-element". With a zero-element of @z@ and a weight-combination
    --  function @weightComb@ then for all @a@: @weightComb z a = a@.
    -- E.g.: equal to 0 if @weightComb@ equals @('+')@ and 1 if @weightComb@ equals @('*')@.
    -> weight
    -- ^ "Infinity"-element: no path with a "length" (cumulative weight) greater than or equal to this will be found.
    --
    -- So when using e.g. 'Data.Int.Int64' as weight, you can safely use 'maxBound' here as long as you're not interested in finding paths with a length of 'maxBound'.
    -> BF s v weight meta a
    -> ST s a
runBF graph weightCombine isLessThan zero infinity = do
    BF.runBF
        graph
        (\(Unboxed weight) meta -> Unboxed $ weightCombine weight meta)
        (\(Unboxed a) (Unboxed b) -> isLessThan a b)
        (Unboxed zero)
        (Unboxed infinity)

-- | NB: has no effect if the source vertex does not exist
bellmanFord
    :: (Eq weight, Show weight, Ord v, Hashable v, Show v, Show meta, Eq meta, Unboxable weight s)
    => v    -- ^ Source vertex
    -> BF s v weight meta ()
bellmanFord =
    BF.bellmanFord

-- | NB: returns 'Nothing' if the target vertex does not exist
pathTo
    :: (Show v, Eq v, Hashable v, Show meta, Unboxable weight s)
    => v                        -- ^ Target vertex
    -> BF s v weight meta (Maybe [DG.IdxEdge v meta])
pathTo =
    BF.pathTo

-- | Get negative cycle ('Nothing' in case there's no negative cycle)
negativeCycle
    :: BF s v weight meta (Maybe (NE.NonEmpty (DG.IdxEdge v meta)))
negativeCycle =
    BF.negativeCycle
