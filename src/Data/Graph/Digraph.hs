{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Graph.Digraph
( -- * Graph
  Digraph
, fromEdges
, fromEdgesCombine
, updateEdge
, removeEdge
, vertexCount
, edgeCount
, vertices
, edges
, vertexLabels
, vertexLabelsId
, outgoingEdges
, outgoingEdges'
, incomingEdges
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
import Protolude (NFData(rnf), Generic)
import qualified Data.Graph.Edge as E
import qualified Data.Graph.Util as U

import qualified Data.Array.ST as Arr
import qualified Data.Array.IArray as IArr
import qualified Data.HashTable.ST.Basic as HT
import Data.Ix (Ix(..))

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
                    -- srcVertexId -> (dstVertexId -> outgoingEdge)
    {-# UNPACK #-} !(Arr.STArray s VertexId (HT.HashTable s VertexId (IdxEdge v meta)))
                    -- v -> vertexId
    {-# UNPACK #-} !(HT.HashTable s v VertexId)

-- |
fromEdges
    :: (Eq v, Ord v, Hashable v, E.DirectedEdge edge v meta)
    => [edge]
    -> ST s (Digraph s v meta)
fromEdges = fromEdgesCombine (const id)

-- | Same as 'fromEdges', but offers a way to combine a new
--    and existing edge with the same "from" and "to" vertex.
fromEdgesCombine
    :: (Eq v, Ord v, Hashable v, E.DirectedEdge edge v a)
    => (Maybe meta -> a -> meta) -- ^ "existing -> new -> combined"
    -> [edge]   -- ^ (meta, from, to)
    -> ST s (Digraph s v meta)
fromEdgesCombine combine edges' = do
    -- Create vertex->index map
    indexMap <- HT.newSized vertexCount' :: ST s (HT.HashTable s v VertexId)
    -- Initialize vertex-index map
    forM_ (zip uniqueVertices [0..]) $ \(v, idx) -> HT.insert indexMap v (VertexId idx)
    -- Initialize vertex array
    outEdgeMapList <- sequence $ replicate vertexCount' HT.new
    vertexArray <- Arr.newListArray (VertexId 0, VertexId (vertexCount'-1)) outEdgeMapList
    -- Populate vertex array
    mapM_ (insertEdge_ combine vertexArray indexMap) edges'
    return $ Digraph vertexCount' vertexArray indexMap
  where
    vertexCount' = length uniqueVertices
    uniqueVertices = U.nubOrd $ map E.fromNode edges' ++ map E.toNode edges'

-- | An immutable form of 'Digraph'
data IDigraph v meta = IDigraph !Int !(IArr.Array VertexId [(VertexId, IdxEdge v meta)]) ![(v, VertexId)]

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
    kvArray <- sequence $ fmap keyValueSet frozenArray
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
    indexMap <- fromKvSet indexKv
    return $ Digraph vc vertexArray indexMap
  where
    fromKvSet kvSet = do
        ht <- HT.new
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

-- |
insertEdge_
    :: E.DirectedEdge edge v a
    => (Maybe meta -> a -> meta) -- ^ "existing -> new -> combined"
    -> Arr.STArray s VertexId (HT.HashTable s VertexId (IdxEdge v meta))
    -> HT.HashTable s v VertexId
    -> edge
    -> ST s ()
insertEdge_ combine vertexArray indexMap edge = do
    fromIdx <- lookup' from
    toIdx <- lookup' to
    outEdgeMap <- Arr.readArray vertexArray fromIdx
    let mkIdxEdge metaData = IdxEdge { eMeta = metaData, _eFrom = from, _eTo = to, _eFromIdx = fromIdx, _eToIdx = toIdx }
    HT.mutate outEdgeMap toIdx (mutateFunc mkIdxEdge)
  where
    mutateFunc mkIdxEdge edgeM =
        let toMeta = combine (fmap E.metaData edgeM)
        in (Just $ mkIdxEdge (toMeta $ E.metaData edge), ())
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

-- | All the edges in the graph
edges
    :: Digraph s v meta
    -> ST s [IdxEdge v meta]
edges (Digraph _ vertexArray _) =
    concat <$> (Arr.getElems vertexArray >>= mapM valueSet)

-- | All the vertex labels in the graph
vertexLabels
    :: Digraph s v meta
    -> ST s [v]
vertexLabels (Digraph _ _ indexMap) =
    keySet indexMap

-- | Same as 'vertexLabels' but includes the vertex 'VertexId'
vertexLabelsId
    :: Digraph s v meta
    -> ST s [(v, VertexId)]
vertexLabelsId (Digraph _ _ indexMap) =
    keyValueSet indexMap

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

-- | WARNING: O(n) running time where "n" is the number of edges
incomingEdges
    :: Digraph s v meta
    -> VertexId
    -> ST s [IdxEdge v meta]
incomingEdges (Digraph _ vertexArray _) vid = do
    assocs <- Arr.getAssocs vertexArray
    foldM (\lst (_, ht) -> HT.foldM htFold lst ht) [] assocs
  where
    htFold accum (k, v)
        | k == vid = return $ v : accum
        | otherwise = return accum

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
