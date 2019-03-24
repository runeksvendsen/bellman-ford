module Data.Graph.Digraph
( -- * Types
  Digraph
  -- * Building
, new
, fromEdges
, insertVertex
, lookupVertex
, insertEdge
, mapEdges
  -- * Queries
, vertexCount
, vertices
, vertexLabels
, outgoingEdges
  -- * Re-exports
, DirectedEdge(..)
)
where

import           Data.Graph.Prelude
import           Data.Graph.Edge                    (DirectedEdge(..))
import           Data.Graph.Orphans                 ()
import qualified Data.Queue                         as Q
import qualified Data.Graph.Mutable                 as Mut
import           Data.Graph.Types                   (Vertex)
import           Data.Graph.Types.Internal          ( MGraph(MGraph, mgraphVertexIndex)
                                                    , mgraphCurrentId, Vertex(Vertex)
                                                    )
import           Data.Hashable                      (Hashable)
import qualified Data.HashMap.Mutable.Basic         as HM
import qualified Data.Primitive.MutVar              as MV


-- | A graph with directed edges
data Digraph s g e v = Digraph
    { -- | The underlying graph
      dgGraph      :: !(MGraph s g e v)
      -- | A map from a vertex id to its outgoing edges
    , dgOutEdges   :: !(HM.MHashMap s (Vertex g) (Q.MQueue s e))
    }

instance Show (Digraph s g e v) where
    show = const "Digraph"

new :: (PrimMonad m)
    => m (Digraph (PrimState m) g e v)
new =
    Digraph <$> mgraph <*> HM.new
  where
    mgraph = MGraph <$> HM.new <*> MV.newMutVar 0 <*> HM.new

fromEdges
    :: (PrimMonad m, DirectedEdge e v)
    => [e]
    -> m (Digraph (PrimState m) g e v)
fromEdges edges = do
    graph <- new
    forM_ edges (insertEdge graph)
    return graph

-- | Insert vertex by label
insertVertex
    :: (PrimMonad m, Eq v, Hashable v)
    => Digraph (PrimState m) g e v
    -> v
    -> m (Vertex g)
insertVertex (Digraph g _) =
    Mut.insertVertex g

-- | Look up vertex by label
lookupVertex
    :: (PrimMonad m, Eq v, Hashable v)
    => Digraph (PrimState m) g e v
    -> v
    -> m (Maybe (Vertex g))
lookupVertex (Digraph g _) =
    Mut.lookupVertex g

insertEdge
    :: (PrimMonad m, DirectedEdge e v)
    => Digraph (PrimState m) g e v
    -> e
    -> m ()
insertEdge graph@(Digraph _ outEdgeMap) edge = do
    fromVertex <- insertVertex graph (fromNode edge)
    _          <- insertVertex graph (toNode edge)
    edgeQueueM <- HM.lookup outEdgeMap fromVertex
    case edgeQueueM of
        Nothing ->
            Q.singleton edge >>= HM.insert outEdgeMap fromVertex
        Just queue ->
            Q.enqueue queue edge

-- | Count of the number of vertices in the graph
vertexCount
    :: (PrimMonad m)
    => Digraph (PrimState m) g e v  -- ^ Graph
    -> m Word                       -- ^ Vertex count
vertexCount (Digraph graph _) =
    fromIntegral <$> MV.readMutVar (mgraphCurrentId graph)

-- | All the vertices in the graph
vertices
    :: (PrimMonad m)
    => Digraph (PrimState m) g e v  -- ^ Graph
    -> m [Vertex g]                 -- ^ List of vertices in the graph
vertices (Digraph graph _) = do
    currId <- MV.readMutVar (mgraphCurrentId graph)
    return $ fmap Vertex [0..currId-1]

-- | All the vertex labels in the graph
vertexLabels
    :: (PrimMonad m)
    => Digraph (PrimState m) g e v  -- ^ Graph
    -> m [v]                 -- ^ List of vertices in the graph
vertexLabels (Digraph graph _) =
    HM.foldM (\accum v _ -> return $ v : accum) [] (mgraphVertexIndex graph)

-- | All edges going out of the given vertex
outgoingEdges
    :: (PrimMonad m)
    => Digraph (PrimState m) g e v  -- ^ Graph
    -> Vertex g                     -- ^ Vertex
    -> m [e]
outgoingEdges (Digraph _ outEdgeMap) vertex = do
    edgeQueueM <- HM.lookup outEdgeMap vertex
    maybe (return []) Q.toList edgeQueueM

-- | Copy a graph, modifying its edges as specified.
-- NB: will only contain the vertices specified by the new edges.
mapEdges
    :: (PrimMonad m, DirectedEdge e' v)
    => (e -> m e')
    -> Digraph (PrimState m) g e v
    -> m (Digraph (PrimState m) g e' v)
mapEdges f graph = do
    vertexList <- vertices graph
    newGraph <- new
    forM_ vertexList $ \vertex -> do
        outEdges <- outgoingEdges graph vertex
        forM_ outEdges (f >=> insertEdge newGraph)
    return newGraph
