{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Translation of Sedgewick & Wayne's @EdgeWeightedDigraph.java@ to Haskell (https://algs4.cs.princeton.edu/44sp/EdgeWeightedDigraph.java.html).
--
--  Note that this supports only a single edge between two given vertices. In order to support multiple edges between two given vertices, you must change the graph's edge type to support it -- e.g. by using an edge type @NonEmpty e@ instead of @e@, and merging all edges that the same source and destination vertex into this non-empty list (which is what 'fromEdgesMulti' does).
module Data.Graph.Digraph
( -- * Graph
  Digraph
, fromEdges
, fromIdxEdges
, fromEdgesMulti
, updateEdge
, insertEdge
, removeEdge
, vertexCount
, edgeCount
, vertices
, vertexLabels
, veticesAndLabels
, outgoingEdges
, outgoingEdges'
, lookupVertex
, lookupVertexReverseSlowTMP
, freeze
, thaw
, flipGraphEdges
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
, flipEdge
  -- * Util
, graphToDot, graphToDotMulti
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
import qualified Data.Text.Lazy as LT
import Data.Bifunctor (first)

------------------------------------------------------------------
------------------  Edge with indexed vertices  ------------------
------------------------------------------------------------------

-- | An edge with indexed vertices
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

-- | Swap the from/to vertices of the edge.
--
--   E.g. the edge @"a"->"b"@ will become the edge @"b"->"a"@.
flipEdge
    :: IdxEdge v meta
    -> IdxEdge v meta
flipEdge edge =
    edge
    { _eFrom = _eTo edge
    , _eTo = _eFrom edge
    , _eFromIdx = _eToIdx edge
    , _eToIdx = _eFromIdx edge
    }

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
    { -- | vertex count
      digraphVertexCount :: {-# UNPACK #-} !Int
      -- | vertexId -> (dstVertexId -> outgoingEdge)
      --
      -- NOTE: A 'HT.HashTable' is used instead of another array to avoid n² memory usage (where n = vertex count)
    , digraphVertexArray :: {-# UNPACK #-} !(Arr.STArray s VertexId (HT.HashTable s VertexId (IdxEdge v meta)))
      -- | v -> vertexId
    , digraphVertexIndex :: {-# UNPACK #-} !(HT.HashTable s v VertexId)
    }

fromEdges
    :: (Eq v, Ord v, Hashable v, E.DirectedEdge edge v meta)
    => [edge]
    -> ST s (Digraph s v meta)
fromEdges edges = do
    -- Create vertex->index map
    indexMap <- HT.newSized vertexCount' :: ST s (HT.HashTable s v VertexId)
    -- Initialize vertex-index map
    forM_ (zip uniqueVertices [0..]) $ \(v, idx) -> HT.insert indexMap v (VertexId idx)
    -- Populate vertex array
    idxEdges <- mapM (createIdxEdge indexMap) edges
    fromIdxEdges_ vertexCount' indexMap idxEdges
  where
    vertexCount' = length uniqueVertices
    uniqueVertices = U.nubOrd $ map E.fromNode edges ++ map E.toNode edges

-- | An optimization of 'fromEdges' for 'IdxEdge' edges
fromIdxEdges
    :: (Hashable v, Ord v)
    => [IdxEdge v meta]
    -> ST s (Digraph s v meta)
fromIdxEdges idxEdges = do
    -- Create vertex->index map
    indexMap <- HT.newSized vertexCount' :: ST s (HT.HashTable s v VertexId)
    -- Initialize vertex-index map
    forM_ uniqueVertices (uncurry $ HT.insert indexMap)
    fromIdxEdges_ vertexCount' indexMap idxEdges
  where
    vertexCount'
        | null idxEdges = 0
        | otherwise = 1 + vidInt (maximum $ map snd uniqueVertices)
    uniqueVertices = U.nubOrd $
        map (\e -> (eFrom e, eFromIdx e)) idxEdges ++
        map (\e -> (eTo e, eToIdx e)) idxEdges

fromIdxEdges_
    :: Int
    -> HT.HashTable s v VertexId
    -> [IdxEdge v meta]
    -> ST s (Digraph s v meta)
fromIdxEdges_ vertexCount' indexMap idxEdges = do
    -- Initialize vertex array
    outEdgeMapList <- replicateM vertexCount' HT.new
    vertexArray <- Arr.newListArray (VertexId 0, VertexId (vertexCount' - 1)) outEdgeMapList
    -- Populate vertex array
    mapM_ (insertIdxEdge vertexArray) idxEdges
    return $ Digraph vertexCount' vertexArray indexMap

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

-- | Return a copy of the input graph where 'flipEdge' has been applied to all edges in the graph
flipGraphEdges
    :: Digraph s v meta
    -> ST s (Digraph s v meta)
flipGraphEdges g = do
    vertexList <- vertices g
    edges <- forM vertexList (outgoingEdges g)
    fromIdxEdges_ (digraphVertexCount g) (digraphVertexIndex g) (map flipEdge $ concat edges)

-- | An immutable form of 'Digraph'
data IDigraph v meta =
    IDigraph
        !Int
        -- ^ Vertex count
        !(IArr.Array VertexId ([(VertexId, IdxEdge v meta)], Int))
        -- ^ Outgoing edges array, including list length
        ![(v, VertexId)] -- TODO: !(IArr.Array v VertexId)
        -- ^ vertex-to-VertexId map
            deriving (Eq, Show)

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
    kvArray <- mapM keyValueSetWithLength frozenArray
    kvSet <- keyValueSet indexMap
    -- NOTE: This is done so that two 'Digraph's containing the same edges and
    --       same vertices (with the same vertex indices) will produce 'IDigraph's
    --       that are equal (==).
    --       If this step is left out, then the insertion order into the
    --       'HT.HashTable' will affect whether or not the resulting two
    --       'IDigraph's are equal.
    let kvArray' = first (sortOn fst) <$> kvArray
    return $ IDigraph vc kvArray' kvSet

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

createIdxEdge
    :: E.DirectedEdge edge v meta
    => HT.HashTable s v VertexId
    -> edge
    -> ST s (IdxEdge v meta)
createIdxEdge indexMap edge = do
    fromIdx <- lookup' from
    toIdx <- lookup' to
    let idxEdge = IdxEdge { eMeta = E.metaData edge, _eFrom = from, _eTo = to, _eFromIdx = fromIdx, _eToIdx = toIdx }
    pure idxEdge
  where
    from = E.fromNode edge
    to = E.toNode edge
    lookup' = fmap (fromMaybe (error "BUG: lookup indexMap")) . HT.lookup indexMap

lookupVertexReverseSlowTMP
    :: Digraph s v meta
    -> VertexId
    -> ST s (Maybe v)
lookupVertexReverseSlowTMP dg vid = do
    let ht = digraphVertexIndex dg
    size <- HT.size ht
    kvList <- keyValueSet ht
    map' <- HT.newSized size
    forM_ kvList $ \(k, v) ->
        HT.insert map' v k
    HT.lookup map' vid

-- | Look up vertex by label
lookupVertex
    :: (Eq v, Hashable v)
    => Digraph s v meta
    -> v
    -> ST s (Maybe VertexId)
lookupVertex (Digraph _ _ indexMap) vertex = do
    HT.lookup indexMap vertex

insertIdxEdge
    :: Arr.STArray s VertexId (HT.HashTable s VertexId (IdxEdge v meta))
    -> IdxEdge v meta
    -> ST s ()
insertIdxEdge vertexArray idxEdge@IdxEdge{..} = do
    outEdgeMap <- Arr.readArray vertexArray _eFromIdx
    HT.insert outEdgeMap _eToIdx idxEdge

-- Overwrite an existing edge in the graph
updateEdge
    :: Digraph s v meta
    -> IdxEdge v meta
    -> ST s ()
updateEdge (Digraph _ vertexArray _) = insertIdxEdge vertexArray

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
    countList <- mapM HT.size outEdgeMapList
    return $ fromIntegral $ sum countList

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

-- | All the vertices in the graph including vertex labels
veticesAndLabels
    :: Digraph s v meta
    -> ST s [(v, VertexId)]
veticesAndLabels (Digraph _ _ indexMap) =
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

-- | TODO: better name
graphToDot
    :: (v -> LT.Text)
    -- ^ Vertex label
    -> (IdxEdge v meta -> Set.Set LT.Text)
    -- ^ Edge labels.
    --
    -- This is a 'Set.Set' because a 'Digraph' supports only a single edge between two specific vertices.
    -- So if you use e.g. a 'NE.NonEmpty' of edges as the graph metadata to get around this,
    -- then you'll want this function to produce one label for each edge in this non-empty list.
    -> LT.Text
    -- ^ Graph name
    -> Digraph s v meta
    -> ST s LT.Text
graphToDot mkVertexLabel mkEdgeLabel gLabel g = do
    res <- veticesAndLabels g
    nodesAndEdges <- forM res $ \(v, vid) -> do
        edges <- outgoingEdges g vid
        pure ((v, vid), edges)
    pure $ mkGraph nodesAndEdges
  where
    showVertexId = LT.pack . show . vidInt
    escape = LT.replace "\"" "\\\"\\"

    mkNode (v, idx) = statement $ LT.unwords
        [ showVertexId idx
        , bracketize $ mkLabel (mkVertexLabel v)
        ]

    mkEdge idxEdge edgeLabel =
        statement $ LT.unwords
            [ showVertexId $ eFromIdx idxEdge
            , "->"
            , showVertexId $ eToIdx idxEdge
            , bracketize $ mkLabel edgeLabel
            ]

    mkEdges idxEdge =
        map (mkEdge idxEdge) (Set.toList $ mkEdgeLabel idxEdge)

    mkNodeAndEdges (vertex, idxEdges) =
          mkNode vertex
        : concatMap mkEdges idxEdges

    mkGraph nodesAndEdges = mkDigraph $ LT.unlines $
          statement (mkLabel gLabel)
        : concatMap mkNodeAndEdges nodesAndEdges

    mkDigraph txt = "digraph {\n" <> txt <> "\n}"
    mkLabel lbl = "label = " <> quote (escape lbl)
    quote txt = "\"" <> txt <> "\""
    bracketize txt = "[" <> txt <> "]"
    statement txt = txt <> ";"

-- | Same as 'graphToDot' but specialized to graphs that use a 'NE.NonEmpty' as graph metadata,
--   as produced by e.g. 'fromEdgesMulti'.
graphToDotMulti
    :: forall v meta s.
       (v -> LT.Text)
    -> (IdxEdge v meta -> LT.Text)
    -> LT.Text
    -> Digraph s v (NE.NonEmpty meta)
    -> ST s LT.Text
graphToDotMulti mkVertexLabel mkEdgeLabel =
    graphToDot mkVertexLabel mkEdgeLabel'
  where
    conv :: IdxEdge v (NE.NonEmpty meta) -> NE.NonEmpty (IdxEdge v meta)
    conv edge =
        let nonEmptyMetas = eMeta edge
            mkEdge meta = edge{ eMeta = meta }
        in mkEdge <$> nonEmptyMetas

    mkEdgeLabel' :: IdxEdge v (NE.NonEmpty meta) -> Set.Set LT.Text
    mkEdgeLabel' = Set.fromList . map mkEdgeLabel . NE.toList . conv

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
