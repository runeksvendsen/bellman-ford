-- | "Data.Graph.BellmanFord" specialized to 'Double' edge weights
module Data.Graph.BellmanFord.Double
( -- * Monad
  runBF
, BF
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
import qualified Data.Graph.SP.Double as DoubleSP
import           Prelude                            hiding (cycle)
import           Data.Graph.Prelude
import           Data.Graph.IsWeight
import qualified Data.Graph.Digraph                 as DG
import qualified Data.Graph.Edge                    as E
import qualified Data.List.NonEmpty                 as NE

type BF s v meta = BF.BF s v (Unboxed Double) meta


-- |
runBF
    :: DG.Digraph s v meta
    -> (Double -> meta -> Double)
    -- ^ Weight combination function @f@.
    --   @f a b@ calculates a new distance to a /to/-vertex.
    --   @a@ is the distance to the edge's /from/-vertex,
    --    and @b@ is the edge going from the /from/-vertex to the /to/-vertex.
    --   If the value returned by this
    --    function is less than the current distance to /to/ the distance to /to/ will
    --    be updated.
    --  E.g. for Dijkstra with type parameter @e@ equal to 'Double',
    --   this function would simply be @('+')@.
    -> Double
    -- ^ "Zero-element". With a zero-element of @z@ and a weight-combination
    --  function @weightComb@ then for all @a@: @weightComb z a = a@.
    -- E.g.: equal to 0 if @weightComb@ equals @('+')@ and 1 if @weightComb@ equals @('*')@.
    -> BF s v meta a
    -> ST s a
runBF graph weightCombine zero = do
    BF.runBF
        graph
        (\(Unboxed weight) meta -> Unboxed $ weightCombine weight meta)
        DoubleSP.isLessThan
        (Unboxed zero)
        (Unboxed (1/0))

-- | NB: has no effect if the source vertex does not exist
bellmanFord
    :: (Ord v, Hashable v, Show v, Show meta, Eq meta)
    => v    -- ^ Source vertex
    -> BF s v meta ()
bellmanFord =
    BF.bellmanFord

-- | NB: returns 'Nothing' if the target vertex does not exist
pathTo
    :: (Show v, Eq v, Hashable v, Show meta)
    => v                        -- ^ Target vertex
    -> BF s v meta (Maybe [DG.IdxEdge v meta])
pathTo =
    BF.pathTo

-- | Get negative cycle ('Nothing' in case there's no negative cycle)
negativeCycle
    :: BF s v meta (Maybe (NE.NonEmpty (DG.IdxEdge v meta)))
negativeCycle =
    BF.negativeCycle
