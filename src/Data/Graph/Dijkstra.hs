{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Graph.Dijkstra
( -- * Monad
  runDijkstra
, Dijkstra
  -- * Algorithm
, dijkstra
, dijkstraSourceSink
  -- * Queries
, pathTo
  -- * Types
, E.DirectedEdge(..)
  -- * Extras
, getGraph
)
where

import           Prelude                            hiding (cycle)
import           Data.Graph.Prelude
import qualified Data.Graph.Digraph                 as DG
import qualified Data.Graph.Edge                    as E
import           Data.Array.ST                      (STArray, STUArray)
import qualified Data.IndexMinPQ as Q
import qualified Data.Array.MArray                  as Arr
import qualified Control.Monad.Reader               as R
import           Data.Ix                            (range)
import Unsafe.Coerce (unsafeCoerce)

type Dijkstra s v meta = R.ReaderT (State s v meta) (ST s)

-- |
runDijkstra
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
    -> Dijkstra s v meta a
    -> ST s a
runDijkstra graph weightCombine zero bf = do
    -- TODO: assert all edge weights >= 0
    mutState <- initState graph
    let state = State graph weightCombine zero mutState
    R.runReaderT bf state

getGraph
    :: Dijkstra s v meta (DG.Digraph s v meta)
getGraph = R.asks sGraph

data State s v meta = State
    { sGraph            :: DG.Digraph s v meta
    , sWeightCombine    :: (Double -> meta -> Double)
    , sZero             :: Double
    , sMState           :: MState s v meta
    }

-- |
data MState s v meta = MState
    { -- | distTo[v] = distance of shortest s->v path
      distTo    :: STUArray s Int Double
      -- | edgeTo[v] = last edge on shortest s->v path
    , edgeTo    :: STArray s Int (Maybe (DG.IdxEdge v meta))
      -- | queue of vertices to relax
    , queue     :: Q.IndexMinPQ s Double
    }

-- | Necessary because of floating point rounding errors.
--   Cf. https://stackoverflow.com/a/65051801/700597
epsilon :: Double
epsilon = 1.0e-14

-- | Reset state in 'MState' so that it's the same as returned by 'initState'
resetState
    :: MState s g e
    -> Dijkstra s v meta ()
resetState mutState = R.lift $ do
    fillArray (distTo mutState) (1/0)
    fillArray (edgeTo mutState) Nothing
    emptyQueue (queue mutState)
  where
    emptyQueue
        :: Q.IndexMinPQ s Double
        -> ST s ()
    emptyQueue = void . Q.emptyAsSortedList

    fillArray
        :: Arr.MArray a e (ST s)
        => a Int e
        -> e
        -> (ST s) ()
    fillArray arr value = do
        indices <- range <$> Arr.getBounds arr
        forM_ indices (\idx -> Arr.writeArray arr idx value)

-- | NB: has no effect if the source vertex does not exist
dijkstra
    :: (Ord v, Hashable v, Show v, Show meta, Eq meta)
    => v    -- ^ Source vertex
    -> Dijkstra s v meta ()
dijkstra = dijkstraTerminate (const $ pure False)

-- | Source-sink shortest path
--
-- Find _only_ the shortest path from @source@ to @destination@,
-- not all shortests paths starting from @source@.
dijkstraSourceSink
    :: (Ord v, Hashable v, Show v, Show meta, Eq meta)
    => (v, v)    -- ^ (source vertex, destination vertex)
    -> Dijkstra s v meta ()
dijkstraSourceSink (src, dst) = do
    graph <- R.asks sGraph
    mVid <- R.lift $ DG.lookupVertex graph dst
    forM_ mVid $ \vid ->
        dijkstraTerminate (\vid' -> pure $ vid' == vid) src

-- | NB: has no effect if the source vertex does not exist
dijkstraTerminate
    :: (Ord v, Hashable v, Show v, Show meta, Eq meta)
    => (DG.VertexId -> Dijkstra s v meta Bool)
    -> v    -- ^ Source vertex
    -> Dijkstra s v meta ()
dijkstraTerminate terminate src = do
    graph <- R.asks sGraph
    state <- R.asks sMState
    srcVertexM <- R.lift (DG.lookupVertex graph src)
    forM_ srcVertexM (initAndGo state graph)
  where
    initAndGo state graph srcVertex = do
        resetState state
        zero <- R.asks sZero
        R.lift $ Arr.writeArray (distTo state) (DG.vidInt srcVertex) zero
        R.lift $ enqueueVertex state srcVertex zero
        go state graph

    go state graph = do
        let pq = queue state
        whenM (not <$> R.lift (Q.isEmpty pq)) $ do
            v <- dequeueVertex
            unlessM (terminate v) $ do
                edgeList <- R.lift $ DG.outgoingEdges graph (unsafeCoerce v) -- TODO: avoid unsafeCoerce
                forM_ edgeList relax
                go state graph

{-# SCC relax #-}
-- |
relax
    :: (Show v, Ord v, Hashable v, Show meta)
    => DG.IdxEdge v meta
    -> Dijkstra s v meta ()
relax edge = do
    calcWeight <- R.asks sWeightCombine
    state      <- R.asks sMState
    distToFrom <- distTo' (DG.eFromIdx edge)
    handleEdge state calcWeight distToFrom
  where
    handleEdge state calcWeight distToFrom = do
        let to = DG.eToIdx edge
            toInt = DG.vidInt to
        -- Look up current distance to "to" vertex
        distToTo <- distTo' to
        -- Actual relaxation
        let newToWeight = calcWeight distToFrom (DG.eMeta edge)
        when (distToTo > newToWeight + epsilon) $ R.lift $ do
            Arr.writeArray (distTo state) toInt newToWeight -- distTo[w] = distTo[v] + e.weight()
            Arr.writeArray (edgeTo state) toInt (Just edge) -- edgeTo[w] = e
            queueContainsToNode <- Q.contains (queue state) toInt
            if queueContainsToNode -- if (pq.contains(w))
                then Q.decreaseKey (queue state) toInt newToWeight -- pq.decreaseKey(w, distTo[w])
                else enqueueVertex state to newToWeight -- pq.insert(w, distTo[w])

distTo'
    :: DG.VertexId
    -> Dijkstra s v meta Double
distTo' v = do
    state <- R.asks sMState
    R.lift $ Arr.readArray (distTo state) (DG.vidInt v)

-- | NB: returns 'Nothing' if the target vertex does not exist
pathTo
    :: (Show v, Eq v, Hashable v, Show meta)
    => v                        -- ^ Target vertex
    -> Dijkstra s v meta (Maybe [DG.IdxEdge v meta])
pathTo target =
    foldPathTo (\edge accum -> edge : accum) [] target

-- | Fold over the edges in the shortest path to a vertex
foldPathTo
    :: forall v meta accum s.
       (Show v, Eq v, Hashable v, Show meta)
    => (DG.IdxEdge v meta -> accum -> accum)
    -- ^ Fold function: first argument is an edge on the shortest path to the target vertex
    -> accum
    -- ^ Initial state for fold (@accum@)
    -> v
    -- ^ Target vertex
    -> Dijkstra s v meta (Maybe accum)
    -- ^ Returns 'Nothing' if either the target vertex doesn't exist or there is no path to it
foldPathTo folder initAccum target = do
    graph <- R.asks sGraph
    targetVertexM <- R.lift (DG.lookupVertex graph target)
    maybe (return Nothing) findPath targetVertexM
  where
    findPath targetVertex = do
        state <- R.asks sMState
        pathExists <- hasPathTo targetVertex
        R.lift $ if pathExists
            then Just <$> go state initAccum targetVertex
            else return Nothing

    go :: MState s v meta -> accum -> DG.VertexId -> ST s accum
    go state accum toVertex = do
        edgeM <- Arr.readArray (edgeTo state) (DG.vidInt toVertex)
        case edgeM of
            Nothing   -> return accum
            Just edge -> do
                go state (folder edge accum) (DG.eFromIdx edge)


hasPathTo
    :: DG.VertexId
    -> Dijkstra s v meta Bool
hasPathTo target =
    (< (1/0)) <$> distTo' target

-- | Create initial 'MState'
initState
    :: DG.Digraph s v meta   -- ^ Graph
    -> ST s (MState s g e)   -- ^ Initialized state
initState graph = do
    vertexCount <- fromIntegral <$> DG.vertexCount graph
    MState
        <$> Arr.newArray (0, vertexCount) (1/0)      -- distTo
        <*> Arr.newArray (0, vertexCount) Nothing    -- edgeTo
        <*> Q.newIndexMinPQ vertexCount                                           -- queue

-- | Add vertex to queue (helper function)
enqueueVertex
    :: MState s g e
    -> DG.VertexId
    -> Double
    -> ST s ()
enqueueVertex state vertex dist = do
    Q.insert (queue state) (DG.vidInt vertex) dist

-- | Remove vertex from queue (helper function)
dequeueVertex
    :: Dijkstra s v meta DG.VertexId
dequeueVertex = do
    state <- R.asks sMState
    unsafeCoerce <$> R.lift (Q.delMin (queue state))

-- | check optimality conditions: either
-- (i) there exists a negative cycle reachable from s
--     or
-- (ii)  for all edges e = v->w:            distTo[w] <= distTo[v] + e.weight()
-- (ii') for all edges e = v->w on the SPT: distTo[w] == distTo[v] + e.weight()
check
    :: (Eq meta, Show meta, Show v, Eq v)
    => Int
    -> Dijkstra s v meta Bool
check source = do
    graph      <- R.asks sGraph
    zero       <- R.asks sZero
    calcWeight <- R.asks sWeightCombine
    state      <- R.asks sMState
    check' state graph zero calcWeight
    return True
  where
    check' state graph zero calcWeight = R.lift $ do
        -- check that distTo[v] and edgeTo[v] are consistent
        whenM ((/= zero) <$> Arr.readArray (distTo state) source) $
            error "distTo source /= zero"
        whenM ((/= Nothing) <$> Arr.readArray (edgeTo state) source) $
            error "edgeTo source /= null"
        -- check that: edgeTo[v] == null implies distTo[v] == infinity and vice versa
        vertices <- DG.vertices graph
        forM_ (map DG.vidInt vertices) $ \v ->
            unless (v == source) $ do
                edgeToV <- Arr.readArray (edgeTo state) v
                distToV <- Arr.readArray (distTo state) v
                when (edgeToV == Nothing && distToV /= 1/0) $
                    error $ "distTo[] and edgeTo[] inconsistent: " ++ show (edgeToV, distToV)
        -- check that all edges e = v->w satisfy distTo[w] <= distTo[v] + e.weight()
        forM_ vertices $ \v -> do
            adj <- DG.outgoingEdges graph v
            (flip mapM_) adj $ \e -> do
                let w = DG.eToIdx e
                distToV <- Arr.readArray (distTo state) (DG.vidInt v)
                distToW <- Arr.readArray (distTo state) (DG.vidInt w)
                when (calcWeight distToV (DG.eMeta e) + epsilon < distToW) $
                    error $ "edge " ++ show e ++ " not relaxed"
        -- check that all edges e = v->w on SPT satisfy distTo[w] == distTo[v] + e.weight()
        forM_ (map DG.vidInt vertices) $ \w -> do
            edgeM <- Arr.readArray (edgeTo state) w
            case edgeM of
                Nothing -> return ()
                Just e  -> do
                    let toVertex = DG.vidInt $ DG.eToIdx e
                    when (w /= toVertex) $
                        error $ "edgeTo[v].to /= v"
                    let v = DG.vidInt $ DG.eFromIdx e
                    distToV <- Arr.readArray (distTo state) v
                    distToW <- Arr.readArray (distTo state) w
                    when (calcWeight distToV (DG.eMeta e) /= distToW) $
                        error $ "edge " ++ show e ++ " on shortest path not tight"
