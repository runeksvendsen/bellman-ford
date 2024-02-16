{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Graph.Dijkstra
( -- * Monad
  runDijkstra, runDijkstraTrace, runDijkstraTraceGeneric
, Dijkstra
  -- * Algorithm
, dijkstra
, dijkstraSourceSink
, dijkstraSourceSinkSamePrio
, dijkstraTerminateDstPrio
, dijkstraKShortestPaths
, dijkstraShortestPathsLevels
  -- * Types
, E.DirectedEdge(..)
, TraceEvent(..)
  -- * Extras
, getGraph
)
where

import           Prelude                            hiding (cycle)
import           Data.Graph.Prelude
import           Data.Graph.SP.Types
import qualified Data.Graph.Digraph                 as DG
import qualified Data.Graph.Edge                    as E
import           Data.Array.ST                      (STUArray)
import qualified Data.MinPQ as Q
import qualified Data.Array.MArray                  as Arr
import qualified Control.Monad.Reader               as R
import Debug.Trace (traceM)
import qualified Data.STRef as Ref
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.STRef as ST

type Dijkstra s v meta = R.ReaderT (State s v meta) (ST s)

type MyList a = [a]

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
runDijkstra =
    runDijkstraTraceGeneric $ const (pure ())

-- | Same as 'runDijkstra' but print tracing information
runDijkstraTrace
    :: (Show meta, Show v)
    => DG.Digraph s v meta
    -> (Double -> meta -> Double)
    -> Double
    -> Dijkstra s v meta a
    -> ST s a
runDijkstraTrace =
    runDijkstraTraceGeneric $ traceM . renderTraceEvent

-- | Same as 'runDijkstra' but provide a function that will receive a 'TraceEvent' when certain events occur during the execution of the algorithm.
runDijkstraTraceGeneric
    :: (TraceEvent v meta Double -> ST s ())
    -> DG.Digraph s v meta
    -> (Double -> meta -> Double)
    -> Double
    -> Dijkstra s v meta a
    -> ST s a
runDijkstraTraceGeneric traceFun graph weightCombine zero action = do
    -- TODO: assert all edge weights >= 0
    mutState <- initState graph
    let state = State traceFun graph weightCombine zero mutState
    R.runReaderT action state

getGraph
    :: Dijkstra s v meta (DG.Digraph s v meta)
getGraph = R.asks sGraph

data State s v meta = State
    { sTrace            :: TraceEvent v meta Double -> ST s ()
    , sGraph            :: DG.Digraph s v meta
    , sWeightCombine    :: (Double -> meta -> Double)
    , sZero             :: Double
    , sMState           :: MState s v meta
    }

-- | A vertex, along with (1) the path from "src" to the vertex; and (2) the distance of this path
data QueueItem v meta = QueueItem
    {-# UNPACK #-} !DG.VertexId -- ^ vertex
    {-# UNPACK #-} !Double -- ^ weight of path to vertex
    !(MyList (DG.IdxEdge v meta)) -- ^ path to vertex

-- | Uses only 'queueItem_weight'
instance Eq (QueueItem v meta) where
    QueueItem _ w1 _ == QueueItem _ w2 _ = w1 == w2

-- | Uses only 'queueItem_weight'
instance Ord (QueueItem v meta) where
    QueueItem _ w1 _ <= QueueItem _ w2 _ = w1 <= w2

-- |
newtype MState s v meta = MState
    { queue     :: Q.MinPQ s (QueueItem v meta)
    }

-- | Reset state in 'MState' so that it's the same as returned by 'initState'
resetState
    :: MState s g e
    -> Dijkstra s v meta ()
resetState mutState = R.lift $ do
    emptyQueue (queue mutState)
  where
    emptyQueue
        :: Ord item => Q.MinPQ s item -> ST s ()
    emptyQueue = Q.empty

-- | NB: has no effect if the source vertex does not exist
dijkstra
    :: (Ord v, Hashable v, Show v, Show meta, Eq meta)
    => v    -- ^ Source vertex
    -> Dijkstra s v meta ()
dijkstra = dijkstraTerminate (const $ const $ const $ pure RelaxOutgoingEdges)

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
        dijkstraTerminate (\vid' _ _ -> pure $ if vid' == vid then Terminate else RelaxOutgoingEdges) src

-- | Terminate when a vertex is dequeued whose priority
--   is greater than the priority of the destination vertex
dijkstraSourceSinkSamePrio
    :: (Ord v, Hashable v, Show v, Show meta, Eq meta)
    => (v, v)    -- ^ (source vertex, destination vertex)
    -> Dijkstra s v meta ()
dijkstraSourceSinkSamePrio =
    dijkstraTerminateDstPrio $ \_ prio dstPrio -> pure $ if prio /= dstPrio then Terminate else RelaxOutgoingEdges

-- | Same as 'dijkstraTerminate' but the termination function includes the priority
--   of the destination vertex.
--   This means the termination function isn't executed until the priority of the
--   destination vertex is known.
dijkstraTerminateDstPrio
    :: (Ord v, Hashable v, Show v, Show meta, Eq meta)
    => (DG.VertexId -> Double -> Double -> Dijkstra s v meta QueuePopAction)
       -- ^ Terminate when this function returns 'True'.
       --   Args:
       --     (1) dequeued vertex
       --     (2) priority of dequeued vertex
       --     (3) priority of destination vertex
    -> (v, v)
       -- ^ (source vertex, destination vertex)
    -> Dijkstra s v meta ()
dijkstraTerminateDstPrio fTerminate (src, dst) = do
    graph <- R.asks sGraph
    mVid <- R.lift $ DG.lookupVertex graph dst
    prioRef <- R.lift $ Ref.newSTRef (1/0)
    forM_ mVid $ \vid -> do
        let terminate vid' prio _ = do
                when (vid' == vid) $ do
                    R.lift $ Ref.writeSTRef prioRef prio
                dstPrio <- R.lift $ Ref.readSTRef prioRef
                if dstPrio == (1/0)
                    then pure RelaxOutgoingEdges
                    else fTerminate vid' prio dstPrio
        dijkstraTerminate terminate src

--- | WIP: 'k' shortest paths
dijkstraKShortestPaths
    :: (Ord v, Hashable v, Show v, Show meta, Eq meta)
    => Int
       -- ^ Maximum number of shortest paths to return
    -> (v, v)
       -- ^ (source vertex, destination vertex)
    -> Dijkstra s v meta (Maybe [([DG.IdxEdge v meta], Double)])
dijkstraKShortestPaths =
    dijkstraShortestPaths (const $ const $ const $ pure False)

-- | Find /n/ sets of shortests paths, where each set contains shortests paths of the same length.
dijkstraShortestPathsLevels
    :: forall s v meta.
       (Ord v, Hashable v, Show v, Show meta, Eq meta)
    => Int -- ^ maximum number of shortest paths to find.
           --   used to put an upper bound on the running time.
           --   terminates after this number of shortests path have been found in total.
    -> Int -- ^ maximum number of "levels" to find (number of sets).
           --   level 0: find only the first shortest path
           --   level 1: find all the shortest paths with the same length as the first shortest path
           --   level 2: find all the shortest paths with a length up to that of the second shortest path
           --   level 3: find all the shortest paths with a length up to that of the third shortest path
           --   ...
           --   level n: find all the shortest paths with a length up to that of the /n/ shortest path
    -> (v, v)
    -> Dijkstra s v meta (Maybe [([DG.IdxEdge v meta], Double)])
dijkstraShortestPathsLevels k numLevels srcDst@(_, dst) = withDstVid $ \dstVid -> do
    shortestPathLengthRef <- R.lift $ ST.newSTRef (1/0 :: Double) -- length of the first shortest path
    lastFoundPathLengthRef <- R.lift $ ST.newSTRef (1/0 :: Double) -- length of the most recent shortest path
    levelCountRef <- R.lift $ ST.newSTRef (0 :: Int)
    let fEarlyTerminate u prio _ = R.lift $ do
            done <- areWeDone prio
            when (u == dstVid) $
                foundPathToDst prio
            pure done

        foundPathToDst prio = do
            shortestPathLength <- ST.readSTRef shortestPathLengthRef
            when (shortestPathLength == 1/0) $
                ST.writeSTRef shortestPathLengthRef prio
            checkUpdateLevelCount prio
            ST.writeSTRef lastFoundPathLengthRef prio

        -- we are done when we have the requested number of levels and
        -- a vertex is popped whose priority is higher than the priority
        -- of the most recently found shortest path
        areWeDone prio = do
            lastFoundPathLength <- ST.readSTRef lastFoundPathLengthRef
            levelCount <- ST.readSTRef levelCountRef
            pure $ prio /= lastFoundPathLength
                && levelCount >= numLevels

        checkUpdateLevelCount prio = do
            lastFoundPathLength <- ST.readSTRef lastFoundPathLengthRef
            when (prio /= lastFoundPathLength) $
                ST.modifySTRef' levelCountRef (+1)

    dijkstraShortestPaths fEarlyTerminate k srcDst
  where
    withDstVid
        :: (DG.VertexId -> Dijkstra s v meta (Maybe a))
        -> Dijkstra s v meta (Maybe a)
    withDstVid f = do
        graph <- R.asks sGraph
        mDstVid <- R.lift $ DG.lookupVertex graph dst
        join <$> forM mDstVid f

--- | WIP: 'k' shortest paths with pre-termination
dijkstraShortestPaths
    :: (Ord v, Hashable v, Show v, Show meta, Eq meta)
    => (DG.VertexId -> Double -> MyList (DG.IdxEdge v meta) -> Dijkstra s v meta Bool)
       -- ^ Return 'True' to terminate before /k/ paths have been found.
       --   The arguments to this function are the same as those of the function passed to 'dijkstraTerminate'
    -> Int
       -- ^ Maximum number of shortest paths to return (/k/)
    -> (v, v)
       -- ^ (source vertex, destination vertex)
    -> Dijkstra s v meta (Maybe [([DG.IdxEdge v meta], Double)])
dijkstraShortestPaths fEarlyTerminate k (src, dst) = do
    graph <- R.asks sGraph
    trace' <- R.asks sTrace
    mDstVid <- R.lift $ DG.lookupVertex graph dst
    resultRef <- R.lift $ Ref.newSTRef []
    -- "count" array, cf. "Algorithm 1" https://codeforces.com/blog/entry/102085.
    -- Keeps track of how many times each vertex has been relaxed.
    count <- R.lift $ do
        vertexCount <- fromIntegral <$> DG.vertexCount graph
        Arr.newArray (0, vertexCount) 0
    forM mDstVid $ \dstVid -> do
        dijkstraTerminate (fTerminate' trace' count resultRef dstVid) src
        R.lift $ Ref.readSTRef resultRef
  where
    fTerminate' trace' count resultRef dstVid u prio pathToU = do
        earlyTerminate <- fEarlyTerminate u prio pathToU
        if earlyTerminate
            then pure Terminate
            else fTerminate trace' count resultRef dstVid u prio pathToU

    fTerminate trace' count resultRef dstVid u prio pathToU = R.lift $ do
        tCount <- Arr.readArray count (DG.vidInt dstVid) -- count[t]
        if tCount < k
            then do
                uCount <- Arr.readArray count (DG.vidInt u) -- count[u]
                if uCount >= k
                    then pure SkipRelax
                    else do
                        let path' = reverse pathToU
                        when (u == dstVid) $ do
                            -- The first edge of the path must start at 'src'
                            unless (maybe True (\firstEdge -> DG.eFrom firstEdge == src) (listToMaybe path')) $
                                error $ "dijkstraTerminate: first edge of shortest path doesn't start at 'src': " <> show path'
                            accumResult resultRef path' prio
                            void $ trace' $ TraceEvent_FoundPath (uCount + 1) prio path'
                        incrementCount count u
                        pure RelaxOutgoingEdges
            else pure Terminate

    accumResult resultRef path prio = do
        Ref.modifySTRef' resultRef ((path, prio) :)

    -- count[u] += 1
    incrementCount :: STUArray s Int Int -> DG.VertexId -> ST s ()
    incrementCount count u = do
        vCount <- Arr.readArray count (DG.vidInt u)
        Arr.writeArray count (DG.vidInt u) (unsafeCoerce $ vCount + 1)

data QueuePopAction
    = RelaxOutgoingEdges
    | SkipRelax
    | Terminate
        deriving (Eq, Show, Ord)

-- | NB: has no effect if the source vertex does not exist
dijkstraTerminate
    :: forall v meta s.
       (Ord v, Hashable v, Show v, Show meta, Eq meta)
    => (DG.VertexId -> Double -> MyList (DG.IdxEdge v meta) -> Dijkstra s v meta QueuePopAction)
    -- ^ Terminate when this function returns 'True'.
    -- ^ Args:
    --     (1) dequeued vertex (@u@)
    --     (2) priority of dequeued vertex
    --     (3) reversed list of edges going from @src@ to @u@.
    --         the first edge in the list points /to/ @u@ while the last edge in the list points /from/ @src@.
    --         apply 'reverse' to this list to get a list of edges going from @src@ to @u@.
    -> v
    -- ^ Source vertex @src@
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
        calcWeight <- R.asks sWeightCombine
        trace' <- R.asks sTrace
        R.lift $ trace' $ TraceEvent_Init (src, srcVertex) zero
        R.lift $ enqueueVertex state (srcVertex, []) zero
        let calcPathLength :: MyList (DG.IdxEdge v meta) -> Double
            calcPathLength = foldr (flip calcWeight . DG.eMeta) zero
        go calcPathLength (queue state) graph trace'
        R.lift $ trace' $ TraceEvent_Done (src, srcVertex)

    go calcPathLength pq graph trace' = do
        mPrioV <- R.lift $ Q.pop pq
        forM_ mPrioV $ \(QueueItem v prio pathTo') -> do
            unless (calcPathLength pathTo' == prio) $
                error $ "dijkstraTerminate: prio /= length path. Prio: " <> show prio <> " path: " <> show pathTo'
            mV <- R.lift $ DG.lookupVertexId graph v
            let v' = fromMaybe (error "oops") mV
            _ <- R.lift $ trace' $ TraceEvent_Pop v' prio pathTo'
            queuePopAction <- terminate v prio pathTo'
            when (queuePopAction == RelaxOutgoingEdges) $ do
                edgeList <- R.lift $ DG.outgoingEdges graph v
                forM_ edgeList (relax pathTo' prio)
            unless (queuePopAction == Terminate) $
                go calcPathLength pq graph trace'

{-# SCC relax #-}
-- |
relax
    :: (Show v, Ord v, Hashable v, Show meta)
    => MyList (DG.IdxEdge v meta) -- ^ path from source to the edge's "from" vertex
    -> Double -- ^ distance from source to the edge's "from" vertex
    -> DG.IdxEdge v meta -- ^ edge to relax
    -> Dijkstra s v meta ()
relax pathTo' distToFrom edge = do
    calcWeight <- R.asks sWeightCombine
    state      <- R.asks sMState
    trace' <- R.asks sTrace
    handleEdge state calcWeight trace'
  where
    handleEdge state calcWeight trace' = do
        let to = DG.eToIdx edge
            newToWeight = calcWeight distToFrom (DG.eMeta edge)
        -- push (l + w, (edge :, v))
        _ <- R.lift $ trace' $ TraceEvent_Push edge newToWeight pathTo'
        R.lift $ enqueueVertex state (to, edge : pathTo') newToWeight

-- | Create initial 'MState'
initState
    :: DG.Digraph s v meta   -- ^ Graph
    -> ST s (MState s g e)   -- ^ Initialized state
initState graph = do
    vertexCount <- fromIntegral <$> DG.vertexCount graph
    MState
        <$> Q.new vertexCount

-- | Add vertex to queue (helper function)
enqueueVertex
    :: MState s g e
    -> (DG.VertexId, MyList (DG.IdxEdge g e))
    -> Double
    -> ST s ()
enqueueVertex state (v, pathTo) dist = do
    Q.push (queue state) $ QueueItem v dist pathTo
