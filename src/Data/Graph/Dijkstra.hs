{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
module Data.Graph.Dijkstra
( -- * Monad
  runDijkstra, runDijkstraTrace, runDijkstraTraceGeneric
, Dijkstra
  -- * Algorithm
, dijkstraKShortestPaths
, dijkstraShortestPathsLevels
, dijkstraShortestPathsLevelsAccum
, dijkstraShortestPathsLevelsTimeout, TimeBoundedResult(..), timeBoundedResultListToList
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
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.STRef as ST
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Monad.ST.Unsafe
import qualified Data.Time
import qualified System.Timeout
import qualified Control.Concurrent.Async
import qualified Streaming.Prelude as S
import Control.Monad.ST.Class (MonadST(..))

type Dijkstra s v meta = R.ReaderT (Env s v meta) (ST s)

type MonadDijkstra v meta m =
    ( R.MonadReader (Env (World m) v meta) m
    , MonadST m
    )

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
    runDijkstraTraceGeneric $ \mTraceEvent ->
        mTraceEvent >>= traceM . renderTraceEvent

-- | Same as 'runDijkstra' but provide a function that will receive a 'TraceEvent' when certain events occur during the execution of the algorithm.
runDijkstraTraceGeneric
    :: (ST s (TraceEvent v meta Double) -> ST s ())
    -> DG.Digraph s v meta
    -> (Double -> meta -> Double)
    -> Double
    -> Dijkstra s v meta a
    -> ST s a
runDijkstraTraceGeneric traceFun graph weightCombine zero action = do
    -- TODO: assert all edge weights >= 0
    mutState <- initState graph
    let state = Env traceFun graph weightCombine zero mutState
    R.runReaderT action state

getGraph
    :: Dijkstra s v meta (DG.Digraph s v meta)
getGraph = R.asks sGraph

data Env s v meta = Env
    { sLiftTrace        :: ST s (TraceEvent v meta Double) -> ST s ()
    , sGraph            :: DG.Digraph s v meta
    , sWeightCombine    :: Double -> meta -> Double
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
    -> ST s ()
resetState mutState =
    emptyQueue (queue mutState)
  where
    emptyQueue
        :: Ord item => Q.MinPQ s item -> ST s ()
    emptyQueue = Q.empty

--- | Find /k/ shortest paths.
dijkstraKShortestPaths
    :: (Ord v, Hashable v, Show v, Show meta, Eq meta)
    => Int
       -- ^ Maximum number of shortest paths to return
    -> (DG.VertexId, DG.VertexId)
       -- ^ (source vertex, destination vertex)
    -> Dijkstra s v meta [([DG.IdxEdge v meta], Double)]
       -- ^ List of: (@path@, @path length@). @path length@ is monotonically increasing.
dijkstraKShortestPaths =
    dijkstraShortestPaths (const $ const $ const $ pure False) (\result -> pure . (result :)) []

-- | Find /n/ sets of shortests paths, where each set contains shortests paths of the same length.
--
--   Returns paths in the same order as 'dijkstraKShortestPaths', but returns /all/ shortest paths of the same length.
--   'dijkstraShortestPathsLevels' with argument @levels = 0@ is equivalent to 'dijkstraKShortestPaths 1'.
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
    -> (DG.VertexId, DG.VertexId)
    -- ^ (source vertex, destination vertex)
    -> Dijkstra s v meta [([DG.IdxEdge v meta], Double)]
    -- ^ List of: (@path@, @path length@). @path length@ is monotonically increasing.
dijkstraShortestPathsLevels k numLevels srcDst = reverse <$>
    dijkstraShortestPathsLevelsAccum (\result -> pure . (result :)) [] k numLevels srcDst

-- | A result produced by 'dijkstraShortestPathsLevelsTimeout'
data TimeBoundedResult a
    = TimeBoundedResult_Result a -- ^ A result
    | TimeBoundedResult_Done -- ^ No more results. Finished within the time limit.
    | TimeBoundedResult_TimedOut -- ^ Timed out. Did not finish within the time limit.
        deriving (Eq, Show, Ord, Functor)

timeBoundedResultListToList
    :: [TimeBoundedResult a]
    -> [a]
timeBoundedResultListToList =
    mapMaybe $ \case
        TimeBoundedResult_Result a -> Just a
        TimeBoundedResult_Done -> Nothing
        TimeBoundedResult_TimedOut -> Nothing

-- | Same as 'dijkstraShortestPathsLevels' but limit running time.
--
--   Results are provided as a 'S.Stream'.
--
--   TODO: Create a proper streaming implementation instead of this hack around 'dijkstraShortestPathsLevelsAccum'.
dijkstraShortestPathsLevelsTimeout
    :: (Ord v, Hashable v, Show v, Show meta, Eq meta)
    => (forall b. Dijkstra RealWorld v meta b -> ST RealWorld b) -- ^ Run 'Dijkstra' action
    -> Int -- ^ /k/
    -> Int -- ^ /levels/
    -> (DG.VertexId, DG.VertexId) -- ^ (src, dst)
    -> Data.Time.NominalDiffTime -- ^ Time limit (negative means "wait indefinitely")
    -> S.Stream (S.Of (TimeBoundedResult ([DG.IdxEdge v meta], Double))) IO ()
dijkstraShortestPathsLevelsTimeout runner k numLevels srcDst timeout = do
    chan <- do
        chan <- R.lift Chan.newChan
        queryAsync <- R.lift $ Control.Concurrent.Async.async $ runTimeLimitedQueryIO chan
        _ <- R.lift $ Control.Concurrent.Async.async $ writeResultOnTimeout queryAsync chan
        pure chan
    go chan
    where
        go chan =
            R.lift (Chan.readChan chan) >>= \case
                res@TimeBoundedResult_Result{} -> S.yield res >> go chan
                other -> S.yield other

        runTimeLimitedQueryIO chan =
            let timeoutMicros = ceiling $ Data.Time.nominalDiffTimeToSeconds timeout * 1e6
            in System.Timeout.timeout timeoutMicros $ void $ stToIO $ runner $
                dijkstraShortestPathsLevelsAccum
                    (\result () ->
                        Control.Monad.ST.Unsafe.unsafeIOToST $
                            Chan.writeChan chan (TimeBoundedResult_Result result)
                    )
                    ()
                    k
                    numLevels
                    srcDst

        writeResultOnTimeout queryAsync chan = do
            timeBoundedResult <- Control.Concurrent.Async.wait queryAsync >>= \case
                Nothing -> pure TimeBoundedResult_TimedOut
                Just () -> pure TimeBoundedResult_Done
            Chan.writeChan chan timeBoundedResult

-- | Same as 'dijkstraShortestPathsLevels' but with a custom result accumulator.
--
--   Useful for e.g.:
--     * Returning results as e.g. a stream
--     * Limiting running time using 'System.Timeout.timeout' while returning the results accumulated before the timeout
dijkstraShortestPathsLevelsAccum
    :: forall m v meta state.
       ( Ord v, Hashable v, Show v, Show meta, Eq meta
       , MonadDijkstra v meta m
       )
    => (([DG.IdxEdge v meta], Double) -> state -> ST (World m) state)
    -- ^ Accumulator function
    -> state -- ^ Initial accumulator state
    -> Int -- ^ max shortest paths count (/k/)
    -> Int -- ^ maximum number of "levels"
    -> (DG.VertexId, DG.VertexId)
    -- ^ (source vertex, destination vertex)
    -> m state
dijkstraShortestPathsLevelsAccum accumResult initalResult k numLevels srcDst@(_, dstVid) = do
    shortestPathLengthRef <- liftST $ ST.newSTRef (1/0 :: Double) -- length of the first shortest path
    lastFoundPathLengthRef <- liftST $ ST.newSTRef (1/0 :: Double) -- length of the most recent shortest path
    levelCountRef <- liftST $ ST.newSTRef (0 :: Int)
    let fEarlyTerminate u prio _ = liftST $ do
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

    dijkstraShortestPaths fEarlyTerminate accumResult initalResult k srcDst

--- | WIP: 'k' shortest paths with pre-termination.
--
--  /t/: destination vertex
--  /u/: vertex popped from queue
--
-- Cf. https://codeforces.com/blog/entry/102085 and https://en.wikipedia.org/wiki/K_shortest_path_routing#Algorithm
dijkstraShortestPaths
    :: ( Ord v, Hashable v, Show v, Show meta, Eq meta
       , MonadDijkstra v meta m
       )
    => (DG.VertexId -> Double -> MyList (DG.IdxEdge v meta) -> m Bool)
       -- ^ Return 'True' to terminate before /k/ paths have been found.
       --   The arguments to this function are the same as those of the function passed to 'dijkstraTerminate'
    -> (([DG.IdxEdge v meta], Double) -> state -> ST (World m) state)
       -- ^ Accumulator function
    -> state
       -- ^ Initial accumulator state
    -> Int
       -- ^ Maximum number of shortest paths to return (/k/)
    -> (DG.VertexId, DG.VertexId)
       -- ^ (source vertex, destination vertex)
    -> m state
dijkstraShortestPaths fEarlyTerminate accumResult initalResult k (srcVid, dstVid) = do
    graph <- R.asks sGraph
    liftTrace <- R.asks sLiftTrace
    -- "count" array, cf. "Algorithm 1" https://codeforces.com/blog/entry/102085.
    -- Keeps track of how many times each vertex has been relaxed.
    count <- liftST $ do
        vertexCount <- fromIntegral <$> DG.vertexCount graph
        Arr.newArray (0, vertexCount) 0
    dijkstraTerminate (fTerminate' liftTrace count) initalResult srcVid
  where
    fTerminate' liftTrace count u prio pathToU state = do
        earlyTerminate <- fEarlyTerminate u prio pathToU
        if earlyTerminate
            then pure (state, Terminate)
            else liftST $ fTerminate liftTrace count u prio pathToU state

    fTerminate liftTrace count u prio pathToU state = do
        tCount <- Arr.readArray count (DG.vidInt dstVid) -- count[t]
        if tCount < k
            then do
                uCount <- Arr.readArray count (DG.vidInt u) -- count[u]
                if uCount >= k
                    then pure (state, SkipRelax)
                    else do
                        let path' = reverse pathToU
                        newState <- if u == dstVid
                            then do
                                -- The first edge of the path must start at 'src'
                                unless (maybe True (\firstEdge -> DG.eFromIdx firstEdge == srcVid) (listToMaybe path')) $
                                    error $ "dijkstraTerminate: first edge of shortest path doesn't start at 'src': " <> show path'
                                () <- liftTrace $ pure $ TraceEvent_FoundPath (uCount + 1) prio path'
                                accumResult (path', prio) state
                            else
                                pure state
                        incrementCount count u
                        pure (newState, RelaxOutgoingEdges)
            else pure (state, Terminate)

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

dijkstraTerminate
    :: forall m v meta state.
       ( Ord v, Hashable v, Show v, Show meta, Eq meta
       , MonadDijkstra v meta m
       )
    => (DG.VertexId -> Double -> MyList (DG.IdxEdge v meta) -> state -> m (state, QueuePopAction))
    -- ^ What to do with a dequeued vertex: (1) relax the edges going out of this vertex (2) don't do anything (3) return @state@.
    --
    -- ^ Args:
    --     (1) dequeued vertex (@u@)
    --     (2) priority of dequeued vertex
    --     (3) reversed list of edges going from @src@ to @u@.
    --         the first edge in the list points /to/ @u@ while the last edge in the list points /from/ @src@.
    --         apply 'reverse' to this list to get a list of edges going from @src@ to @u@.
    -> state
    -> DG.VertexId
    -- ^ Source vertex @src@.
    --
    -- NOTE: If this VertexId does not exist in the given graph and
    --       tracing is on ('runDijkstraTrace' or 'runDijkstraTraceGeneric'),
    --       the vertex labels in 'TraceEvent_Init' and 'TraceEvent_Done' will be /bottom/.
    -> m state
dijkstraTerminate terminate terminateInitState srcVid = do
    graph <- R.asks sGraph
    state <- R.asks sMState
    liftTrace <- R.asks sLiftTrace
    zero <- R.asks sZero
    calcWeight <- R.asks sWeightCombine
    liftST $ initialize state graph zero liftTrace
    let calcPathLength :: MyList (DG.IdxEdge v meta) -> Double
        calcPathLength = foldr (flip calcWeight . DG.eMeta) zero
    finalState <- go calcPathLength (queue state) graph liftTrace terminateInitState
    liftST $ liftTrace $ do
        srcLabel <- lookupVertexIdOrFail graph srcVid
        pure $ TraceEvent_Done (srcLabel, srcVid)
    pure finalState
  where
    initialize
        :: MState s v meta
        -> DG.Digraph s v meta
        -> Double
        -> (ST s (TraceEvent v meta Double) -> ST s ())
        -> ST s ()
    initialize state graph zero liftTrace = do
        resetState state
        liftTrace $ do
            srcLabel <- lookupVertexIdOrFail graph srcVid
            pure $ TraceEvent_Init (srcLabel, srcVid) zero
        enqueueVertex state (srcVid, []) zero

    lookupVertexIdOrFail graph vid = do
        mSrc <- DG.lookupVertexId graph vid
        pure $ fromMaybe (error $ "no such VertexId: " <> show srcVid) mSrc

    go  :: MonadDijkstra v meta m
        => (MyList (DG.IdxEdge v meta) -> Double)
        -> Q.MinPQ (World m) (QueueItem v meta)
        -> DG.Digraph (World m) v meta
        -> (ST (World m) (TraceEvent v meta Double) -> ST (World m) ())
        -> state
        -> m state
    go calcPathLength pq graph liftTrace terminateState = liftST (Q.pop pq) >>= \case
        Nothing -> pure terminateState
        Just (QueueItem v prio pathTo') -> do
            unless (calcPathLength pathTo' == prio) $
                error $ "dijkstraTerminate: prio /= length path. Prio: " <> show prio <> " path: " <> show pathTo'
            liftST $ liftTrace $ do
                vLabel <- lookupVertexIdOrFail graph v
                pure $ TraceEvent_Pop vLabel prio pathTo'
            (newTerminateState, queuePopAction) <- terminate v prio pathTo' terminateState
            let go' = go calcPathLength pq graph liftTrace newTerminateState
                relaxOutgoingEdges = do
                    edgeList <- liftST $ DG.outgoingEdges graph v
                    forM_ edgeList (relax pathTo' prio)
            case queuePopAction of
                RelaxOutgoingEdges -> relaxOutgoingEdges >> go'
                SkipRelax -> go'
                Terminate -> pure newTerminateState

{-# SCC relax #-}
-- |
relax
    :: ( Show v, Ord v, Hashable v, Show meta
       , MonadDijkstra v meta m
       )
    => MyList (DG.IdxEdge v meta) -- ^ path from source to the edge's "from" vertex
    -> Double -- ^ distance from source to the edge's "from" vertex
    -> DG.IdxEdge v meta -- ^ edge to relax
    -> m ()
relax pathTo' distToFrom edge = do
    calcWeight <- R.asks sWeightCombine
    state      <- R.asks sMState
    liftTrace  <- R.asks sLiftTrace
    liftST $ handleEdge state calcWeight liftTrace
  where
    handleEdge state calcWeight liftTrace = do
        let to = DG.eToIdx edge
            newToWeight = calcWeight distToFrom (DG.eMeta edge)
        -- push (l + w, (edge :, v))
        () <- liftTrace $ pure $ TraceEvent_Push edge newToWeight pathTo'
        enqueueVertex state (to, edge : pathTo') newToWeight

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
