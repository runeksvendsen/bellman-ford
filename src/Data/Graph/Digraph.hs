{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

-- | Translation of Sedgewick & Wayne's @EdgeWeightedDigraph.java@ to Haskell (https://algs4.cs.princeton.edu/44sp/EdgeWeightedDigraph.java.html).
--
--  Note that this supports only a single edge between two given vertices. In order to support multiple edges between two given vertices, you must change the graph's edge type to support it -- e.g. by using an edge type @NonEmpty e@ instead of @e@, and merging all edges that the same source and destination vertex into this non-empty list (which is what 'fromEdgesMulti' does).
module Data.Graph.Digraph
( -- * Graph
  Digraph
, fromEdges
, fromEdgesMulti
, updateEdge
, insertEdge
, removeEdge
, vertexCount
, edgeCount
, vertices
, vertexLabels
, outgoingEdges
, outgoingEdges'
, lookupVertex
, freeze
, thaw
, IDigraph
  -- * Edge/vertex
, VertexId
, vidInt
  -- NB: Export setter for the 'eMeta' record field
  --  but only getters for the remaining record fields
, IdxEdge
, eMeta
, eFrom
, eTo
, eFromIdx
, eToIdx
, HasWeight(..)
  -- * Internal
, emptyClone
  -- * Re-exports
, E.DirectedEdge(..)
)
where

import Data.Graph.Prelude
import Protolude (NFData(rnf), Generic, Set)
import qualified Data.Graph.Edge as E
import qualified Data.Graph.Util as U

import qualified Data.Array.ST as Arr
import qualified Data.Array.IArray as IArr
import qualified Data.HashTable.ST.Basic as HT
import Data.Ix (Ix(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.List (sortOn)

------------------------------------------------------------------
------------------  Edge with indexed vertices  ------------------
------------------------------------------------------------------

data IdxEdge v meta = IdxEdge
    { eMeta     :: !meta
    , _eFrom    :: !v
    , _eTo      :: !v
    , _eFromIdx :: {-# UNPACK #-} !VertexId
    , _eToIdx   :: {-# UNPACK #-} !VertexId
    } deriving (Eq, Show, Functor, Generic)

instance (NFData v, NFData meta) => NFData (IdxEdge v meta)

instance (Eq v, Hashable v) => E.DirectedEdge (IdxEdge v meta) v meta where
   fromNode = _eFrom
   toNode = _eTo
   metaData = eMeta

class HasWeight a weight | a -> weight where
    weight :: a -> weight

newtype VertexId = VertexId { _vidInt :: Int }
    deriving (Eq, Show, Ord, Hashable, Ix, NFData)

vidInt :: VertexId -> Int
vidInt = _vidInt

eFrom :: IdxEdge v meta -> v
eFrom = _eFrom

eTo :: IdxEdge v meta -> v
eTo = _eTo

eFromIdx :: IdxEdge v meta -> VertexId
eFromIdx = _eFromIdx

eToIdx :: IdxEdge v meta -> VertexId
eToIdx = _eToIdx


------------------------------------------------------------------
----------------------------  Graph  -----------------------------
------------------------------------------------------------------

data Digraph s v meta = Digraph
                    -- vertex count
    {-# UNPACK #-} !Int
                    -- vertexId -> (dstVertexId -> outgoingEdge)
    {-# UNPACK #-} !(Arr.STArray s VertexId (HT.HashTable s VertexId (IdxEdge v meta)))
                    -- v -> vertexId
    {-# UNPACK #-} !(HT.HashTable s v VertexId)

fromEdges
    :: (Eq v, Ord v, Hashable v, E.DirectedEdge edge v meta)
    => [edge]   -- ^ (meta, from, to)
    -> ST s (Digraph s v meta)
fromEdges edges = do
    -- Create vertex->index map
    indexMap <- HT.newSized vertexCount' :: ST s (HT.HashTable s v VertexId)
    -- Initialize vertex-index map
    forM_ (zip uniqueVertices [0..]) $ \(v, idx) -> HT.insert indexMap v (VertexId idx)
    -- Initialize vertex array
    outEdgeMapList <- sequence $ replicate vertexCount' HT.new
    vertexArray <- Arr.newListArray (VertexId 0, VertexId (vertexCount'-1)) outEdgeMapList
    -- Populate vertex array
    mapM_ (insertEdge_ vertexArray indexMap) edges
    return $ Digraph vertexCount' vertexArray indexMap
  where
    vertexCount' = length uniqueVertices
    uniqueVertices = U.nubOrd $ map E.fromNode edges ++ map E.toNode edges

-- | One or more edges with the same src and dst vertex
data NonEmptyEdges v meta = NonEmptyEdges
    { _NonEmptyEdges_from :: !v
    , _NonEmptyEdges_to :: !v
    , _NonEmptyEdges_meta :: !(NE.NonEmpty meta)
    }

instance (Eq v, Hashable v) => E.DirectedEdge (NonEmptyEdges v meta) v (NE.NonEmpty meta) where
    fromNode = _NonEmptyEdges_from
    toNode   = _NonEmptyEdges_to
    metaData = _NonEmptyEdges_meta

-- | Create a graph that supports multiple edges that share src vertex and dst vertex
fromEdgesMulti
    :: forall s edge v meta.
       (Eq v, Ord v, Hashable v, E.DirectedEdge edge v meta)
    => Set edge
    -> ST s (Digraph s v (NE.NonEmpty meta))
fromEdgesMulti =
    fromEdges
        . map groupedEdgesToNonEmptyEdges
        . groupOn fromTo
  where
    fromTo e = (E.fromNode e, E.toNode e)

    groupOn :: (Ord b) => (a -> b) -> Set a -> [NE.NonEmpty a]
    groupOn f = NE.groupBy (\a1 a2 -> f a1 == f a2) . sortOn f . Set.toList

    -- Invariant: all the edges in the input list share src node and dst node
    groupedEdgesToNonEmptyEdges :: NE.NonEmpty edge -> NonEmptyEdges v meta
    groupedEdgesToNonEmptyEdges edges =
        let edge = NE.head edges
        in NonEmptyEdges (E.fromNode edge) (E.toNode edge) (fmap E.metaData edges)

-- | An immutable form of 'Digraph'
data IDigraph v meta =
    IDigraph
        !Int
        -- ^ Vertex count
        !(IArr.Array VertexId ([(VertexId, IdxEdge v meta)], Int))
        -- ^ Outgoing edges array, including list length
        ![(v, VertexId)] -- TODO: !(IArr.Array v VertexId)
        -- ^ vertex-to-VertexId map

instance (NFData v, NFData meta) => NFData (IDigraph v meta) where
    rnf (IDigraph vc vertexArray indexMap) =
        rnf vc `seq`
        rnf (IArr.assocs vertexArray) `seq`
        rnf indexMap

-- | Convert a mutable graph into an immutable graph.
freeze
    :: Digraph s v meta
    -> ST s (IDigraph v meta)
freeze (Digraph vc vertexArray indexMap) = do
    frozenArray <- Arr.freeze vertexArray
    kvArray <- sequence $ fmap keyValueSetWithLength frozenArray
    kvSet <- keyValueSet indexMap
    return $ IDigraph vc kvArray kvSet

-- | Convert an immutable graph into an mutable graph.
thaw
    :: (Eq v, Hashable v)
    => IDigraph v meta
    -> ST s (Digraph s v meta)
thaw (IDigraph vc frozenArray indexKv) = do
    htArray <- sequence $ fmap fromKvSet frozenArray
    vertexArray <- Arr.thaw htArray
    indexMap <- fromKvSet (indexKv, vc)
    return $ Digraph vc vertexArray indexMap
  where
    fromKvSet (kvSet, size) = do
        ht <- HT.newSized size
        foldM (\ht' (k,v) -> HT.insert ht' k v >> return ht') ht kvSet

-- | Return a copy of the input graph that has the same vertices
--   but with all edges removed.
emptyClone
    :: forall s v meta.
       Digraph s v meta
    -> ST s (Digraph s v meta)
emptyClone (Digraph vc _ indexMap) = do
    emptyMaps <- sequence $ replicate vc HT.new
    newVertexArray <- Arr.newListArray (VertexId 0, VertexId (vc - 1)) emptyMaps
    -- Keeping the same 'indexMap' is safe since it is not modified after graph creation
    return $ Digraph vc newVertexArray indexMap

insertEdge_
    :: E.DirectedEdge edge v meta
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
    :: Digraph s v meta
    -> IdxEdge v meta
    -> ST s ()
updateEdge (Digraph _ vertexArray _) = insertEdge__ vertexArray

-- Insert/overwrite an existing edge in the graph
insertEdge
    :: Digraph s v meta
    -> IdxEdge v meta
    -> ST s ()
insertEdge = updateEdge

-- -- | Remove an existing edge from the graph
removeEdge
    :: Digraph s v meta
    -> IdxEdge v a
    -> ST s ()
removeEdge (Digraph _ vertexArray _) IdxEdge{..} = do
    outEdgeMap <- Arr.readArray vertexArray _eFromIdx
    HT.delete outEdgeMap _eToIdx

-- -- | Count of the number of vertices in the graph
vertexCount
    :: Digraph s v meta
    -> ST s Word
vertexCount (Digraph vc _ _) = return $ fromIntegral vc

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
    vertexIdList = map VertexId [0..vc-1]
    -- NB: If @vc@ equals zero (in case of an empty list passed to 'fromEdges')
    --  and it had the type 'Word', then @vc-1@ would evaluate to 'maxBound'.
    -- Thus @[0..vc-1]@ would produce a list of length 2^32 or 2^64 (depending
    --  on CPU word size).

-- | All the vertex labels in the graph
vertexLabels
    :: Digraph s v meta
    -> ST s [v]
vertexLabels (Digraph _ _ indexMap) =
    keySet indexMap

-- | All edges going out of the given vertex
outgoingEdges
    :: Digraph s v meta
    -> VertexId
    -> ST s [IdxEdge v meta]
outgoingEdges (Digraph _ vertexArray _) vid = do
    outEdgeMap <- Arr.readArray vertexArray vid
    valueSet outEdgeMap

-- | Same as 'outgoingEdges' but takes vertex label as input.
--  Returns 'Nothing' in case the given vertex label does not exist
outgoingEdges'
    :: (Eq v, Hashable v)
    => Digraph s v meta
    -> v
    -> ST s (Maybe [IdxEdge v meta])
outgoingEdges' dg v = do
    vidM <- lookupVertex dg v
    maybe (return Nothing) (fmap Just . outgoingEdges dg) vidM

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

-- | Set of map values
keyValueSet
    :: HT.HashTable s k v
    -> ST s [(k,v)]
keyValueSet =
    HT.foldM (\accum kv -> return $ kv : accum) []

-- | Set of map values
keyValueSetWithLength
    :: HT.HashTable s k v
    -> ST s ([(k,v)], Int)
keyValueSetWithLength =
    HT.foldM (\(accum, !len) kv -> return (kv : accum, len + 1)) ([], 0)
