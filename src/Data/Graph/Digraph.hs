{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Graph.Digraph
( Digraph
, fromEdges
, updateEdge
, removeEdge
, vertexCount
, edgeCount
, vertices
, vertexLabels
, outgoingEdges
, lookupVertex
, VertexId(VertexId)
  -- * Re-exports
, E.DirectedEdge(..)
, module IdxEdge -- TODO: don't export constructor
)
where

import Data.Graph.Prelude
import qualified Data.Graph.Edge as E
import Data.Graph.IdxEdge as IdxEdge

import Data.List (sort, group)
import qualified Data.Array.ST as Arr
import qualified Data.HashTable.ST.Basic as HT


data Digraph s v meta = Digraph
                    -- vertex count
    {-# UNPACK #-} !Word
                    -- vertexId -> (dstVertexId -> outgoingEdge)
    {-# UNPACK #-} !(Arr.STArray s VertexId (HT.HashTable s VertexId (IdxEdge v meta)))
                    -- v -> vid
    {-# UNPACK #-} !(HT.HashTable s v VertexId)

fromEdges
    :: (Eq v, Ord v, Hashable v, DirectedEdge edge v meta)
    => [edge]   -- ^ (meta, from, to)
    -> ST s (Digraph s v meta)
fromEdges edges = do
    -- Create vertex->index map
    indexMap <- HT.newSized vertexCount' :: ST s (HT.HashTable s v VertexId)
    -- Initialize vertex-index map
    forM_ (zip uniqueVertices [1..]) $ \(v, idx) -> HT.insert indexMap v (VertexId idx)
    -- Initialize vertex array
    outEdgeMapList <- sequence $ replicate vertexCount' HT.new
    vertexArray <- Arr.newListArray (VertexId 1, VertexId (vertexCount'+1)) outEdgeMapList
    -- Populate vertex array
    mapM_ (insertEdge_ vertexArray indexMap) edges
    return $ Digraph (fromIntegral vertexCount') vertexArray indexMap
  where
    vertexCount' = length uniqueVertices
    uniqueVertices = map head . group . sort $
        map E.fromNode edges ++ map E.toNode edges

insertEdge_
    :: DirectedEdge edge v meta
    => Arr.STArray s VertexId (HT.HashTable s VertexId (IdxEdge v meta))
    -> HT.HashTable s v VertexId
    -> edge
    -> ST s ()
insertEdge_ vertexArray indexMap edge = do
    fromIdx <- lookup' from
    toIdx <- lookup' to
    outEdgeMap <- Arr.readArray vertexArray fromIdx
    let idxEdge = IdxEdge { eMeta = E.metaData edge, _eFrom = from, _eTo = to, _eFromIdx = fromIdx, _eToIdx = toIdx }
    HT.insert outEdgeMap toIdx idxEdge
  where
    from = E.fromNode edge
    to = E.toNode edge
    lookup' = fmap (fromMaybe (error "BUG: lookup indexMap")) . HT.lookup indexMap

-- | Look up vertex by label
lookupVertex
    :: (Eq v, Hashable v)
    => Digraph s v meta
    -> v
    -> ST s (Maybe VertexId)
lookupVertex (Digraph _ _ indexMap) vertex = do
    HT.lookup indexMap vertex

insertEdge__
    :: Arr.STArray s VertexId (HT.HashTable s VertexId (IdxEdge v meta))
    -> IdxEdge v meta
    -> ST s ()
insertEdge__ vertexArray idxEdge@IdxEdge{..} = do
    outEdgeMap <- Arr.readArray vertexArray _eFromIdx
    HT.insert outEdgeMap _eToIdx idxEdge

-- Overwrite an existing edge in the graph
updateEdge
    :: Arr.STArray s VertexId (HT.HashTable s VertexId (IdxEdge v meta))
    -> IdxEdge v meta
    -> ST s ()
updateEdge = insertEdge__

-- -- | Remove an existing edge from the graph
removeEdge
    :: Digraph s v meta
    -> IdxEdge v meta
    -> ST s ()
removeEdge (Digraph _ vertexArray _) IdxEdge{..} = do
    outEdgeMap <- Arr.readArray vertexArray _eFromIdx
    HT.delete outEdgeMap _eToIdx

-- -- | Count of the number of vertices in the graph
vertexCount
    :: Digraph s v meta
    -> ST s Word
vertexCount (Digraph vc _ _) = return vc

-- | Count of the number of edges in the graph
edgeCount
    :: Digraph s v meta
    -> ST s Word
edgeCount dg@(Digraph _ vertexArray _) = do
    vertexIdList <- vertices dg
    outEdgeMapList <- mapM (Arr.readArray vertexArray) vertexIdList
    countList <- mapM countEdges outEdgeMapList
    return $ sum countList
  where
    countEdges ht =
        HT.foldM (\innerCount _ -> return $ innerCount + 1) 0 ht

-- | All the vertices in the graph
vertices
    :: Digraph s v meta
    -> ST s [VertexId]
vertices (Digraph vc _ _) =
    return vertexIdList
  where
    vertexIdList = map (VertexId . fromIntegral) [1..vc]

-- | All the vertex labels in the graph
vertexLabels
    :: Digraph s v meta
    -> ST s [v]
vertexLabels (Digraph _ _ _) = undefined

-- | All edges going out of the given vertex
outgoingEdges
    :: Digraph s v meta
    -> VertexId
    -> ST s [IdxEdge v meta]
outgoingEdges (Digraph _ vertexArray _) vid = do
    outEdgeMap <- Arr.readArray vertexArray vid
    valueSet outEdgeMap

-- | Set of map values
valueSet
    :: HT.HashTable s k v
    -> ST s [v]
valueSet =
    HT.foldM (\accum (_, v) -> return $ v : accum) []
