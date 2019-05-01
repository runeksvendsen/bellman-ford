{-# LANGUAGE RankNTypes #-}
module Data.Graph.Digraph
( -- * Types
  Digraph
, Vertex
  -- * Building
, new
, withGraph
, fromEdges
, insertVertex
, lookupVertex
, insertEdge
, removeEdge
  -- * Queries
, vertexCount
, edgeCount
, vertices
, vertexLabels
, outgoingEdges
, incomingEdges
  -- * Re-exports
, DirectedEdge(..)
)
where

import           Data.Graph.Prelude
import           Data.Graph.Edge                    (DirectedEdge(..))
import           Data.Graph.Orphans                 ()
import qualified Data.Graph.Mutable                 as Mut
import           Data.Graph.Types                   (Vertex)
import           Data.Graph.Types.Internal          ( MGraph(MGraph, mgraphVertexIndex)
                                                    , mgraphCurrentId, Vertex(Vertex)
                                                    , getVertexInternal
                                                    , IntPair(IntPair)
                                                    )
import           Data.Hashable                      (Hashable)
import qualified Data.HashMap.Mutable.Basic         as HM
import qualified Data.Primitive.MutVar              as MV


-- | A graph with directed edges.
--   Can only contain a single edge from one vertex to another.
data Digraph s g e v = Digraph
    { -- | The underlying graph
      dgGraph      :: !(MGraph s g e v)
      -- | A map from a vertex id to its outgoing edges.
      --   Each outgoing vertex id is mapped to a "(fromNode, toNode) -> edge"-map
      --    in order to increase the speed of looking up a specific edge.
    , dgOutEdges   :: !(HM.MHashMap s (Vertex g) (HM.MHashMap s IntPair e))
      -- | A map from a vertex id to its incoming edges.
    , dgInEdges    :: !(HM.MHashMap s (Vertex g) (HM.MHashMap s IntPair e))
    }

instance Show (Digraph s g e v) where
    show = const "Digraph"

new
    :: (PrimMonad m)
    => m (Digraph (PrimState m) () e v)
new = newInternal

newInternal
    :: (PrimMonad m)
    => m (Digraph (PrimState m) g e v)
newInternal =
    Digraph <$> mgraph <*> HM.new <*> HM.new
  where
    mgraph = MGraph <$> HM.new <*> MV.newMutVar 0 <*> HM.new

-- | Safely work with 'Vertex' types.
--   All 'Vertex'es returned by a function taking the provided
--    'Digraph' as argument can only be used on this same 'Digraph'.
withGraph
    :: PrimMonad m
    => (forall g. Digraph (PrimState m) g e v -> m a)   -- ^ Takes an empty 'Digraph' as argument
    -> m a
withGraph f =
  newInternal >>= f

fromEdges
    :: (PrimMonad m, DirectedEdge e v)
    => [e]
    -> m (Digraph (PrimState m) () e v)
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
insertVertex (Digraph g _ _) =
    Mut.insertVertex g

-- | Look up vertex by label
lookupVertex
    :: (PrimMonad m, Eq v, Hashable v)
    => Digraph (PrimState m) g e v
    -> v
    -> m (Maybe (Vertex g))
lookupVertex (Digraph g _ _) =
    Mut.lookupVertex g

-- | Insert/overwrite edge
insertEdge
    :: (PrimMonad m, DirectedEdge e v)
    => Digraph (PrimState m) g e v  -- ^ Graph
    -> e                            -- ^ Edge to insert/overwrite
    -> m (Vertex g, Vertex g)       -- ^ (from, to)-vertex
insertEdge graph@(Digraph _ outEdgeMap inEdgeMap) edge = do
    fromVertex <- insertVertex graph (fromNode edge)
    toVertex   <- insertVertex graph (toNode edge)
    let intPair = IntPair (getVertexInternal fromVertex) (getVertexInternal toVertex)
    edgeMapInsert outEdgeMap fromVertex intPair
    edgeMapInsert inEdgeMap toVertex intPair
    return (fromVertex, toVertex)
  where
    edgeMapInsert edgeMap vertex edgeKey = do
        vertexEdgeMapM <- HM.lookup edgeMap vertex
        vertexEdgeMap <- case vertexEdgeMapM of
            Nothing -> do
                vertexEdgeMap <- HM.new
                HM.insert edgeMap vertex vertexEdgeMap
                return vertexEdgeMap
            Just vertexEdgeMap -> return vertexEdgeMap
        HM.insert vertexEdgeMap edgeKey edge

-- | Remove edge
removeEdge
    :: (PrimMonad m, DirectedEdge e v)
    => Digraph (PrimState m) g e v  -- ^ Graph
    -> e                            -- ^ Edge to remove
    -> m (Vertex g, Vertex g)       -- ^ (from, to)-vertex
removeEdge graph@(Digraph _ outEdgeMap inEdgeMap) edge = do
    fromVertex <- insertVertex graph (fromNode edge)
    toVertex   <- insertVertex graph (toNode edge)
    let intPair = IntPair (getVertexInternal fromVertex) (getVertexInternal toVertex)
    edgeMapRemove outEdgeMap fromVertex intPair
    edgeMapRemove inEdgeMap toVertex intPair
    return (fromVertex, toVertex)
  where
    edgeMapRemove edgeMap vertex edgeKey = do
        edgeMapM <- HM.lookup edgeMap vertex
        case edgeMapM of
            Nothing            -> return ()
            Just vertexEdgeMap ->
                HM.delete vertexEdgeMap edgeKey

-- | Count of the number of vertices in the graph
vertexCount
    :: (PrimMonad m)
    => Digraph (PrimState m) g e v  -- ^ Graph
    -> m Word                       -- ^ Vertex count
vertexCount (Digraph graph _ _) =
    fromIntegral <$> MV.readMutVar (mgraphCurrentId graph)

-- | Count of the number of edges in the graph
edgeCount
    :: (PrimMonad m)
    => Digraph (PrimState m) g e v  -- ^ Graph
    -> m Word                       -- ^ Edge count
edgeCount (Digraph _ outEdgeMap _) =
    HM.foldM countEdges 0 outEdgeMap
  where
    countEdges count _ = HM.foldM (\innerCount _ _ -> return $ innerCount+1) count

-- | All the vertices in the graph
vertices
    :: (PrimMonad m)
    => Digraph (PrimState m) g e v  -- ^ Graph
    -> m [Vertex g]                 -- ^ List of vertices in the graph
vertices (Digraph graph _ _) = do
    currId <- MV.readMutVar (mgraphCurrentId graph)
    return $ fmap Vertex [0..currId-1]

-- | All the vertex labels in the graph
vertexLabels
    :: (PrimMonad m)
    => Digraph (PrimState m) g e v  -- ^ Graph
    -> m [v]                 -- ^ List of vertices in the graph
vertexLabels (Digraph graph _ _) =
    keySet (mgraphVertexIndex graph)

-- | All edges going out of the given vertex
outgoingEdges
    :: (PrimMonad m)
    => Digraph (PrimState m) g e v  -- ^ Graph
    -> Vertex g                     -- ^ Vertex
    -> m [e]
outgoingEdges (Digraph _ outEdgeMap _) vertex = do
    edgeMapM <- HM.lookup outEdgeMap vertex
    maybe (return []) valueSet edgeMapM

-- | All edges into the given vertex
incomingEdges
    :: (PrimMonad m)
    => Digraph (PrimState m) g e v  -- ^ Graph
    -> Vertex g                     -- ^ Vertex
    -> m [e]
incomingEdges (Digraph _ _ inEdgeMap) vertex = do
    edgeMapM <- HM.lookup inEdgeMap vertex
    maybe (return []) valueSet edgeMapM

-- | Set of map keys
keySet
    :: (PrimMonad m)
    => HM.MHashMap (PrimState m) k v
    -> m [k]
keySet =
    HM.foldM (\accum k _ -> return $ k : accum) []

-- | Set of map values
valueSet
    :: (PrimMonad m)
    => HM.MHashMap (PrimState m) k v
    -> m [v]
valueSet =
    HM.foldM (\accum _ v -> return $ v : accum) []
