{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
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
, incomingEdges
, lookupVertex
, Vertex(Vertex)
  -- * Re-exports
, E.DirectedEdge(..)
, T.Text
)
where

import Data.Graph.Prelude
import qualified Data.Graph.Edge as E

import Data.List (sort, group)
import qualified Data.STRef as ST
import qualified Data.HashTable.ST.Basic as HT
import qualified Data.Text as T
import Data.Ix (Ix(..))


data Digraph s v ek e = Digraph
    {-# UNPACK #-} !Word
    {-# UNPACK #-} !(HT.HashTable s v (ST.STRef s (VertexInfo s v ek e))) -- TODO: remove STRef?
    {-# UNPACK #-} !(HT.HashTable s Int (ST.STRef s (VertexInfo s v ek e))) -- TODO: remove STRef?

-- TODO: check performance effect of UNPACK pragma
data VertexInfo s v ek e = VI
  { viId :: {-# UNPACK #-} !Int -- Starts at 1 and goes up
  , viOutgoing :: {-# UNPACK #-} !(HT.HashTable s (ek,v) e)     -- (ek, Dst) -> edge
  , viIncoming :: {-# UNPACK #-} !(HT.HashTable s (ek,v) e)     -- (ek, Src) -> edge
  }

fromEdges
    :: (E.DirectedEdge e v, Ord v)
    => [e]
    -> ST s (Digraph s v T.Text e)
fromEdges edges = do
    vertexMap <- HT.newSized vertexCount'
    intMap <- HT.newSized vertexCount'
    -- Insert vertex ID's + empty edge maps
    forM_ (zip vertices' [1..]) $ uncurry (insertVertex_ vertexMap intMap)
    -- Populate edge maps
    mapM_ (insertEdge_ vertexMap) edges
    return $ Digraph (fromIntegral vertexCount') vertexMap intMap
  where
    vertexCount' = length vertices'
    vertices' = map head . group . sort $
        map E.fromNode edges ++ map E.toNode edges

insertVertex_
    :: (Eq v, Hashable v)
    => HT.HashTable s v (ST.STRef s (VertexInfo s v ek e))
    -> HT.HashTable s Int (ST.STRef s (VertexInfo s v ek e))
    -> v
    -> Int
    -> ST s ()
insertVertex_ vertexMap intMap vertex vid = do
    outMap <- HT.new
    inMap <- HT.new
    viRef <- ST.newSTRef (VI vid outMap inMap)
    HT.insert vertexMap vertex viRef
    HT.insert intMap vid viRef

-- | Look up vertex by label
lookupVertex
    :: (Eq v, Hashable v)
    => Digraph s v ek e
    -> v
    -> ST s (Maybe (Vertex g))
lookupVertex (Digraph _ vertexMap _) vertex = do
    refM <- HT.lookup vertexMap vertex
    case refM of
        Nothing -> return Nothing
        Just viRef -> do
            vi <- ST.readSTRef viRef
            return $ Just (Vertex $ viId vi)

newtype Vertex g = Vertex Int
    deriving (Eq, Ord, Hashable, Ix)

insertEdge_
    :: (E.DirectedEdge e v)
    => HT.HashTable s v (ST.STRef s (VertexInfo s v T.Text e))
    -> e
    -> ST s ()
insertEdge_ vertexMap edge = do
    lookupInsertEdge E.fromNode viOutgoing E.toNode
    lookupInsertEdge E.toNode viIncoming E.fromNode
  where
    lookupInsertEdge edgeNode edgeMap vertexKey = do
        let vertex = edgeNode edge
        refM <- HT.lookup vertexMap vertex
        case refM of
            Nothing -> error "BUG: insertEdge_: missing vertex"
            Just viRef -> refInsertEdge edgeMap viRef vertexKey
    refInsertEdge edgeMap viRef vertexKey = do
        vi <- ST.readSTRef viRef
        HT.insert (edgeMap vi) (E.multiKey edge, vertexKey edge) edge

-- Overwrite an existing edge in the graph
updateEdge
    :: E.DirectedEdge e v
    => Digraph s v T.Text e
    -> e
    -> ST s ()
updateEdge (Digraph _ ht _) = insertEdge_ ht

-- | Remove an existing edge from the graph
removeEdge
    :: E.DirectedEdge e v
    => Digraph s v T.Text e
    -> e
    -> ST s ()
removeEdge (Digraph _ vertexMap _) edge = do
    lookupRemoveEdge E.fromNode viOutgoing E.toNode
    lookupRemoveEdge E.toNode viIncoming E.fromNode
  where
    lookupRemoveEdge edgeNode edgeMap vertexKey = do
        let vertex = edgeNode edge
        refM <- HT.lookup vertexMap vertex
        case refM of
            Nothing -> error "BUG: removeEdge: missing vertex"
            Just viRef -> refRemoveEdge edgeMap viRef vertexKey
    refRemoveEdge edgeMap viRef vertexKey = do
        vi <- ST.readSTRef viRef
        HT.delete (edgeMap vi) (E.multiKey edge, vertexKey edge)

-- | Count of the number of vertices in the graph
vertexCount
    :: Digraph s v ek e
    -> ST s Word
vertexCount (Digraph vc _ _) = return vc

-- | Count of the number of edges in the graph
edgeCount
    :: Digraph s v ek e
    -> ST s Word
edgeCount (Digraph _ ht _) =
    HT.foldM countEdges 0 ht
  where
    countEdges count (_, refVi) = do
        vi <- ST.readSTRef refVi
        HT.foldM (\innerCount _ -> return $ innerCount+1) count (viOutgoing vi)

-- | All the vertices in the graph
vertices
    :: Digraph s v ek e
    -> ST s [Vertex g]
vertices (Digraph _ ht _) = do
    viList <- valueSet ht >>= sequence . map ST.readSTRef
    return $ map (Vertex . viId) viList

-- | All the vertex labels in the graph
vertexLabels
    :: Digraph s v ek e
    -> ST s [v]
vertexLabels (Digraph _ ht _) = keySet ht

-- | All edges going out of the given vertex
outgoingEdges
    :: Digraph s v ek e
    -> Vertex g
    -> ST s [e]
outgoingEdges (Digraph _ _ intMap) (Vertex vid) = do
    vi <- ST.readSTRef =<< maybe failWithError return =<< HT.lookup intMap vid
    valueSet (viOutgoing vi)
  where
    failWithError = error $ "outgoingEdges: missing vertex " ++ show vid

-- | All edges into the given vertex
incomingEdges
    :: Digraph s v ek e
    -> Vertex g
    -> ST s [e]
incomingEdges (Digraph _ _ intMap) (Vertex vid) = do
    vi <- ST.readSTRef =<< maybe failWithError return =<< HT.lookup intMap vid
    valueSet (viIncoming vi)
  where
    failWithError = error $ "incomingEdges: missing vertex " ++ show vid

-- | Set of map keys
keySet
    :: HT.HashTable s k v
    -> ST s [k]
keySet =
    HT.foldM (\accum (k, _) -> return $ k : accum) []

-- | Set of map values
valueSet
    :: HT.HashTable s k v
    -> ST s [v]
valueSet =
    HT.foldM (\accum (_, v) -> return $ v : accum) []

-- instance Ix (Vertex g) where
--     range (start, end) =
--         fmap Vertex $ range (vIdInternal start, vIdInternal end)
--     index (start, end) sub =
--         index (vIdInternal start, vIdInternal end) (vIdInternal sub)
--     inRange (start, end) sub =
--         inRange (vIdInternal start, vIdInternal end) (vIdInternal sub)
