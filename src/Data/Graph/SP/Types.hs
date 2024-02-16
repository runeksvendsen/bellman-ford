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
    | TraceEvent_Push
      -- ^ An vertex is pushed onto the queue
        !(DG.IdxEdge v meta)
        -- ^ The edge whose /destination/-vertex is pushed onto the queue
        !weight
        -- ^ The priority of the vertex
        ![DG.IdxEdge v meta]
        -- ^ The path that led to the edge's /source/-vertex
    | TraceEvent_Pop
      -- ^ An vertex is pushed onto the queue
        !v
        -- ^ The edge whose /destination/-vertex is pushed onto the queue
        !weight
        -- ^ The priority of the vertex
        ![DG.IdxEdge v meta]
        -- ^ The path that led to the edge's /source/-vertex
    | TraceEvent_FoundPath
      -- ^ Found a shortest path
        !Int
        -- ^ Path number (first found path is number 1)
        !weight
        -- ^ Path length
        ![DG.IdxEdge v meta]
        -- ^ The path

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
    TraceEvent_Push edge weight pathTo -> unwords
        [ "Queued vertex with prio"
        , show weight
        , "to"
        , show (DG.eTo edge) <> "."
        , "Path to vertex:"
        , show pathTo
        ]
    TraceEvent_Pop v weight pathTo -> unwords
        [ "Popped vertex with prio"
        , show weight <> ":"
        , show v <> "."
        , "Path to vertex:"
        , show pathTo
        ]
    TraceEvent_FoundPath number weight path -> unwords
        [ "Found path no."
        , show number
        , "with length"
        , show weight
        , "and edge count"
        , show (length path) <> ":"
        , show path
        ]
