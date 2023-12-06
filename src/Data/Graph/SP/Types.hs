{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Data.Graph.SP.Types
( TraceEvent(..)
, renderTraceEvent
)
where

import Data.Graph.SP.Util
import qualified Data.Graph.Digraph as DG

-- | Contains information about an event that has occurred.
--
-- Can be used to "hook" into the algorithm to collect or print information.
data TraceEvent v meta weight
    = TraceEvent_Relax
      -- ^ An edge is "relaxed", cf. https://algs4.cs.princeton.edu/44sp.
        !(DG.IdxEdge v meta)
        -- ^ The edge that's relaxed
        !weight
        -- ^ The /current/ distance to the edge's /destination/-vertex
    | TraceEvent_Init
      -- ^ 'bellmanFord' is started
        !(v, DG.VertexId)
        -- ^ /source/ vertex
        !weight
        -- ^ The /source/ vertex' distance is initialized to this
    | TraceEvent_Done
      -- ^ 'bellmanFord' has terminated
        !(v, DG.VertexId)
        -- ^ /source/ vertex

renderTraceEvent
    :: (Show meta, Show v, Show weight)
    => TraceEvent v meta weight
    -> String
renderTraceEvent = \case
    TraceEvent_Relax edge distToTo -> unwords
        [ "Relaxing edge", showEdge edge <> "."
        , "Current 'distTo' for"
        , showIndexedVertex (DG.eTo edge, DG.eToIdx edge)
        , "is"
        , show distToTo <> "."
        ]
    TraceEvent_Init srcVertex weight -> unwords
        [ "Starting Bellman-Ford for source vertex"
        , showIndexedVertex srcVertex <> "."
        , "Initializing 'distTo' for"
        , showIndexedVertex srcVertex
        , "to"
        , show weight <> "."
        ]
    TraceEvent_Done srcVertex -> unwords
        [ "Finished Bellman-Ford for source vertex"
        , showIndexedVertex srcVertex
        ]
