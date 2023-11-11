{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Data.Graph.BellmanFord
( -- * Monad
  runBF, runBFTrace, runBFTraceGeneric
, BF
  -- * Algorithm
, bellmanFord
  -- * Queries
, pathTo
, negativeCycle
  -- * Types
, E.DirectedEdge(..)
, TraceEvent(..)
, showIndexedVertex, showEdge
  -- * Extras
, getGraph
  -- * Re-exports
, Boxed(..), Unboxed(..)
)
where

import           Prelude                            hiding (cycle)
import           Data.Graph.Prelude
import           Data.Graph.IsWeight
import qualified Data.Graph.IsWeight                as Weight
import qualified Data.Graph.Digraph                 as DG
import qualified Data.Graph.Edge                    as E
import qualified Data.Graph.Cycle                   as C
import           Data.Array.ST                      (STArray, STUArray)
import qualified Data.Queue                         as Q
import qualified Data.Primitive.MutVar              as MV
import qualified Data.Array.MArray                  as Arr
import qualified Data.List.NonEmpty                 as NE
import qualified Control.Monad.Reader               as R
import           Data.Ix                            (range)
import Debug.Trace (traceM)


type BF s v weight meta = R.ReaderT (State s v weight meta) (ST s)

-- |
runBF
    :: IsWeight weight s
    => DG.Digraph s v meta
    -> (weight -> meta -> weight)
    -- ^ Weight combination function @f@.
    --   @f a b@ calculates a new distance to a /to/-vertex.
    --   @a@ is the distance to the edge's /from/-vertex,
    --    and @b@ is the edge going from the /from/-vertex to the /to/-vertex.
    --   If the value returned by this
    --    function is less than the current distance to /to/ the distance to /to/ will
    --    be updated.
    --  E.g. for Dijkstra with type parameter @e@ equal to 'weight',
    --   this function would simply be @('+')@.
    -> (weight -> weight -> Bool)
    -- ^ @isLessThan@ function. @isLessThan a b@ should return 'True' if @a@ is strictly less than @b@, and 'False' otherwise.
    -> weight
    -- ^ "Zero-element". With a zero-element of @z@ and a weight-combination
    --  function @weightComb@ then for all @a@: @weightComb z a = a@.
    -- E.g.: equal to 0 if @weightComb@ equals @('+')@ and 1 if @weightComb@ equals @('*')@.
    -> weight
    -- ^ "Infinity"-element. @isLessThan a infinityElement@ must return 'True' for all edge weights @a@ in the graph.
    -> BF s v weight meta a
    -> ST s a
runBF = do
    runBFTraceGeneric (const $ pure ())

-- | Contains information about an event that has occurred.
--
-- Can be used to "hook" into the algorithm to collect or print information.
data TraceEvent v meta weight
    = TraceEvent_Relax
      -- ^ An edge is "relaxed", cf. https://algs4.cs.princeton.edu/44sp.
        !(DG.IdxEdge v meta)
        -- ^ The edge that's relaxed
        !weight
        -- ^ The new distance to the edge's /destination/-vertex
    | TraceEvent_Init
      -- ^ 'bellmanFord' is started
        !(v, DG.VertexId)
        -- ^ /source/ vertex
        !weight
        -- ^ The /source/ vertex' distance is initialized to this
    | TraceEvent_Done
      -- ^ 'bellmanFord' has terminated
        !(v, DG.VertexId)
        -- ^ /source/ vertex

-- | Same as 'runBF' but print tracing information
runBFTrace
    :: forall s v meta weight a.
       (IsWeight weight s, Show meta, Show v, Show weight)
    => DG.Digraph s v meta
    -> (weight -> meta -> weight)
    -> (weight -> weight -> Bool)
    -> weight
    -> weight
    -> BF s v weight meta a
    -> ST s a
runBFTrace =
    runBFTraceGeneric $ traceM . trace'
    where
        trace' = \case
            TraceEvent_Relax edge newToWeight -> unwords
                [ "Relaxing edge", showEdge edge <> "."
                , "Updating 'distTo' for"
                , showIndexedVertex (DG.eTo edge, DG.eToIdx edge)
                , "to"
                , show newToWeight <> "."
                ]
            TraceEvent_Init srcVertex weight -> unwords
                [ "Starting Bellman-Ford for source vertex"
                , showIndexedVertex srcVertex <> "."
                , "Initializing 'distTo' for"
                , showIndexedVertex srcVertex
                , "to"
                , show weight <> "."
                ]
            TraceEvent_Done srcVertex -> unwords
                [ "Finished Bellman-Ford for source vertex"
                , showIndexedVertex srcVertex
                ]

showIndexedVertex
    :: Show v
    => (v, DG.VertexId)
    -> String
showIndexedVertex (v, vid) = show (DG.vidInt vid) <> " (" <> show v <> ")"

showEdge
    :: (Show meta, Show v)
    => DG.IdxEdge v meta
    -> String
showEdge e = unwords
    [ showIndexedVertex (DG.eFrom e, DG.eFromIdx e)
    , "->"
    , showIndexedVertex (DG.eTo e, DG.eToIdx e)
    , "(meta:"
    , show (DG.eMeta e) <> ")"
    ]

-- | Same as 'runBF' but provide a function that will receive a 'TraceEvent' when certain events occur during the execution of the algorithm.
runBFTraceGeneric
    :: IsWeight weight s
    => (TraceEvent v meta weight -> ST s ())
    -> DG.Digraph s v meta
    -> (weight -> meta -> weight)
    -> (weight -> weight -> Bool)
    -> weight
    -> weight
    -> BF s v weight meta a
    -> ST s a
runBFTraceGeneric traceFun graph weightCombine isLessThan zero infinity bf = do
    mutState <- initState infinity graph
    let state = State traceFun graph weightCombine isLessThan zero infinity mutState
    R.runReaderT bf state

getGraph
    :: BF s v weight meta (DG.Digraph s v meta)
getGraph = R.asks sGraph

data State s v weight meta = State
    { sTrace            :: TraceEvent v meta weight -> ST s ()
    , sGraph            :: DG.Digraph s v meta
    , sWeightCombine    :: (weight -> meta -> weight)
    , sIsLessThan       :: (weight -> weight -> Bool)
    , sZero             :: weight
    , sInfinity         :: weight
    , sMState           :: MState s v weight meta
    }

-- |
data MState s v weight meta = MState
    { -- | distTo[v] = distance of shortest s->v path
      distTo    :: Array weight s
      -- | edgeTo[v] = last edge on shortest s->v path
    , edgeTo    :: STArray s Int (Maybe (DG.IdxEdge v meta))
      -- | onQueue[v] = is v currently on the queue?
    , onQueue   :: STUArray s Int Bool
      -- | queue of vertices to relax
    , queue     :: Q.MQueue s DG.VertexId
      -- | number of calls to relax()
    , cost      :: MV.MutVar s Word
      -- | negative cycle (empty list if no such cycle)
    , cycle     :: MV.MutVar s [DG.IdxEdge v meta]
    }

-- | Reset state in 'MState' so that it's the same as returned by 'initState'
resetState
    :: IsWeight weight s
    => MState s v weight meta
    -> BF s v weight meta ()
resetState mutState = do
    infinity <- R.asks sInfinity
    R.lift $ do
        fillArrayWeight (distTo mutState) infinity
        fillArray (edgeTo mutState) Nothing
        fillArray (onQueue mutState) False
        emptyQueue (queue mutState)
        MV.atomicModifyMutVar' (cost mutState) (const (0, ()))
        MV.atomicModifyMutVar' (cycle mutState) (const ([], ()))
  where
    emptyQueue
        :: Q.MQueue s DG.VertexId
        -> ST s ()
    emptyQueue queue' = do
        let go = maybe (return ()) (\_ -> Q.dequeue queue' >>= go)
        Q.dequeue queue' >>= go
    fillArray
        :: Arr.MArray a e (ST s)
        => a Int e
        -> e
        -> (ST s) ()
    fillArray arr value = do
        indices <- range <$> Arr.getBounds arr
        forM_ indices (\idx -> Arr.writeArray arr idx value)

    fillArrayWeight
        :: forall weight s.
           IsWeight weight s
        => Array weight s
        -> weight
        -> (ST s) ()
    fillArrayWeight arr value = do
        indices <- range <$> Weight.getBounds @weight arr
        forM_ indices (\idx -> Weight.writeArray arr idx value)

-- | NB: has no effect if the source vertex does not exist
bellmanFord
    :: (Ord v, Hashable v, Show v, Show meta, Eq meta, IsWeight weight s, Eq weight, Show weight)
    => v    -- ^ Source vertex
    -> BF s v weight meta ()
bellmanFord src = do
    trace' <- R.asks sTrace
    graph <- R.asks sGraph
    state <- R.asks sMState
    srcVertexM <- R.lift (DG.lookupVertex graph src)
    forM_ srcVertexM (initAndGo trace' state)
  where
    initAndGo trace' state srcVertex = do
        zero <- R.asks sZero
        _ <- R.lift $ trace' $ TraceEvent_Init (src, srcVertex) zero
        resetState state
        R.lift $ Weight.writeArray (distTo state) (DG.vidInt srcVertex) zero
        R.lift $ enqueueVertex state srcVertex
        go state
        _ <- R.lift $ trace' $ TraceEvent_Done (src, srcVertex)
        (`assert` ()) <$> check (DG.vidInt srcVertex)
    go state = do
        vertexM <- R.lift $ dequeueVertex state
        case vertexM of
            Nothing     -> return ()
            Just vertex -> do
                relax vertex
                -- Recurse unless there's a negative cycle
                unlessM hasNegativeCycle (go state)

-- | NB: returns 'Nothing' if the target vertex does not exist
pathTo
    :: (Show v, Eq v, Hashable v, Show meta, IsWeight weight s)
    => v                        -- ^ Target vertex
    -> BF s v weight meta (Maybe [DG.IdxEdge v meta])
pathTo target = do
    graph <- R.asks sGraph
    infinity <- R.asks sInfinity
    isLessThan <- R.asks sIsLessThan
    targetVertexM <- fmap DG.vidInt <$> R.lift (DG.lookupVertex graph target)
    maybe (return Nothing) (findPath isLessThan infinity graph) targetVertexM
  where
    findPath isLessThan infinity graph targetVertex = do
        negativeCycle >>= maybe (return ()) failNegativeCycle -- Check for negative cycle
        state <- R.asks sMState
        pathExists <- R.lift $ hasPathTo isLessThan infinity state targetVertex
        R.lift $ if pathExists
            then Just <$> go graph state [] targetVertex
            else return Nothing
    failNegativeCycle cycle' =
        error $ "Negative-cost cycle exists (target=" ++ show target ++ "): " ++ show cycle'
    go graph state accum toVertex = do
        edgeM <- Arr.readArray (edgeTo state) toVertex
        case edgeM of
            Nothing   -> return accum
            Just edge -> do
                go graph state (edge : accum) (DG.vidInt $ DG.eFromIdx edge)

hasPathTo
    :: forall weight s v meta m .
       (IsWeight weight s, m ~ ST s)
    => (weight -> weight -> Bool)
    -> weight
    -> MState s v weight meta
    -> Int
    -> ST s Bool
hasPathTo isLessThan infinity state target =
    (`isLessThan` infinity) <$> Weight.readArray @weight (distTo state) target

{-# SCC relax #-}
-- |
relax
    :: (Show v, Ord v, Hashable v, Show meta, IsWeight weight s)
    => DG.VertexId
    -> BF s v weight meta ()
relax vertex = do
    traceRelax <- R.asks sTrace
    graph      <- R.asks sGraph
    calcWeight <- R.asks sWeightCombine
    isLessThan <- R.asks sIsLessThan
    state      <- R.asks sMState
    edgeList   <- R.lift $ DG.outgoingEdges graph vertex
    distToFrom <- R.lift $ Weight.readArray (distTo state) (DG.vidInt vertex)
    vertexCount <- R.lift $ DG.vertexCount graph
    mapM_ (handleEdge traceRelax state calcWeight isLessThan vertexCount distToFrom) edgeList
  where
    handleEdge traceRelax state calcWeight isLessThan vertexCount distToFrom edge =
        unlessM hasNegativeCycle $ do
            let to = DG.eToIdx edge
                toInt = DG.vidInt to
            -- Look up current distance to "to" vertex
            distToTo <- R.lift $ Weight.readArray (distTo state) toInt
            -- Actual relaxation
            let newToWeight = calcWeight distToFrom (DG.eMeta edge)
            when (newToWeight `isLessThan` distToTo) $ R.lift $ do
                _ <- traceRelax $ TraceEvent_Relax edge newToWeight
                Weight.writeArray (distTo state) toInt newToWeight
                Arr.writeArray (edgeTo state) toInt (Just edge)
                unlessM (Arr.readArray (onQueue state) toInt) $
                    enqueueVertex state to
            -- Update cost (number of calls to "relax")
            newCost <- MV.atomicModifyMutVar' (cost state)
                (\cost' -> let newCost = cost' + 1 in (newCost, newCost))
            when (newCost `mod` vertexCount == 0) $
                findNegativeCycle

findNegativeCycle
    :: (Ord v, Show v, Show meta, Hashable v)
    => BF s v weight meta ()
findNegativeCycle = do
    state    <- R.asks sMState
    spEdges  <- R.lift $ Arr.getElems (edgeTo state)
    sptGraph <- mkSptGraph (catMaybes spEdges)
    R.lift $ C.findCycle sptGraph >>= MV.writeMutVar (cycle state)
  where
    -- NB: If we were to create a new graph from the shortest-path edges
    --  using 'DG.fromEdges', then these edges would be re-indexed, and thus have
    --  different indices than the edges in 'sGraph'. This would cause 'negativeCycle'
    --  to return edges with incorrect indices.
    -- This potential bug could be prevented by adding an ST-style phantom
    --  type variable to 'DG.Digraph' and exposing only a @runGraph@ function
    --  in style of e.g. 'Data.Array.ST.runSTArray'. However, this approach has
    --  not been adopted due to the added complexity.
    mkSptGraph spEdges = do
        graph <- getGraph
        sptGraph <- R.lift $ DG.emptyClone graph
        R.lift $ mapM_ (DG.updateEdge sptGraph) spEdges
        return sptGraph

hasNegativeCycle
    :: BF s v weight meta Bool
hasNegativeCycle = do
    state <- R.asks sMState
    fmap (not . null) . MV.readMutVar . cycle $ state

-- | Get negative cycle ('Nothing' in case there's no negative cycle)
negativeCycle
    :: BF s v weight meta (Maybe (NE.NonEmpty (DG.IdxEdge v meta)))
negativeCycle = do
    state    <- R.asks sMState
    edgeList <- MV.readMutVar $ cycle state
    case edgeList of
        []    -> return Nothing
        edges -> return $ Just $ NE.fromList edges

-- | Create initial 'MState'
initState
    :: IsWeight weight s
    => weight
    -> DG.Digraph s v meta   -- ^ Graph
    -> ST s (MState s v weight meta)   -- ^ Initialized state
initState infinity graph = do
    vertexCount <- fromIntegral <$> DG.vertexCount graph
    MState
        <$> Weight.newArray (0, vertexCount) infinity      -- distTo
        <*> Arr.newArray (0, vertexCount) Nothing    -- edgeTo
        <*> Arr.newArray (0, vertexCount) False      -- onQueue
        <*> Q.new                                           -- queue
        <*> MV.newMutVar 0                                  -- cost
        <*> MV.newMutVar []                                 -- cycle

-- | Add vertex to queue (helper function)
enqueueVertex
    :: MState s v weight meta
    -> DG.VertexId
    -> ST s ()
enqueueVertex state vertex = do
    Arr.writeArray (onQueue state) (DG.vidInt vertex) True  -- Mark vertex as being in queue
    Q.enqueue (queue state) vertex              -- Add vertex to queue

-- | Remove vertex from queue (helper function)
dequeueVertex
    :: MState s v weight meta
    -> ST s (Maybe DG.VertexId)
dequeueVertex state = do
    -- Remove vertex from queue
    vertexM <- Q.dequeue (queue state)
    -- Mark vertex as not being in queue
    maybe (return ()) (\vertex -> Arr.writeArray (onQueue state) (DG.vidInt vertex) False) vertexM
    return vertexM

-- | check optimality conditions: either
-- (i) there exists a negative cycle reachable from s
--     or
-- (ii)  for all edges e = v->w:            distTo[w] <= distTo[v] + e.weight()
-- (ii') for all edges e = v->w on the SPT: distTo[w] == distTo[v] + e.weight()
check
    :: (Eq meta, Show meta, Show v, Eq v, Eq weight, Show weight, IsWeight weight s)
    => Int
    -> BF s v weight meta Bool
check source = do
    graph      <- R.asks sGraph
    zero       <- R.asks sZero
    infinity   <- R.asks sInfinity
    calcWeight <- R.asks sWeightCombine
    isLessThan <- R.asks sIsLessThan
    state      <- R.asks sMState
    ifM hasNegativeCycle
        (checkNegativeCycle isLessThan zero calcWeight state)
        (checkNoCycle state graph zero infinity calcWeight isLessThan)
    return True
  where
    checkNegativeCycle isLessThan zero calcWeight state = do
        negativeCycle' <- MV.readMutVar (cycle state)
        -- check that weight of negative cycle is negative
        let weight = foldr (flip calcWeight . DG.eMeta) zero negativeCycle'
        when (zero `isLessThan` weight || weight == zero) $
            error $ unlines [ "negative cycle is non-negative"
                            , printf "weight: %s" (show weight)
                            , printf "edges: %s" (show negativeCycle')
                            ]
    checkNoCycle state graph zero infinity calcWeight isLessThan = R.lift $ do
        -- check that distTo[v] and edgeTo[v] are consistent
        whenM ((/= zero) <$> Weight.readArray (distTo state) source) $
            error "distTo source /= zero"
        whenM ((/= Nothing) <$> Arr.readArray (edgeTo state) source) $
            error "edgeTo source /= null"
        -- check that: edgeTo[v] == null implies distTo[v] == infinity and vice versa
        vertices <- DG.vertices graph
        forM_ (map DG.vidInt vertices) $ \v ->
            unless (v == source) $ do
                edgeToV <- Arr.readArray (edgeTo state) v
                distToV <- Weight.readArray (distTo state) v
                when (edgeToV == Nothing && distToV /= infinity) $
                    error $ "distTo[] and edgeTo[] inconsistent: " ++ show (edgeToV, distToV)
        -- check that all edges e = v->w satisfy distTo[w] <= distTo[v] + e.weight()
        forM_ vertices $ \from -> do
            adj <- DG.outgoingEdges graph from
            (flip mapM_) adj $ \e -> do
                let to = DG.eToIdx e
                    toInt = DG.vidInt to
                    fromInt = DG.vidInt from
                distToFrom <- Weight.readArray (distTo state) fromInt
                distToTo <- Weight.readArray (distTo state) toInt
                let newDistToTo = calcWeight distToFrom (DG.eMeta e)
                -- distTo[from] + e.weight() < distTo[to]
                when (newDistToTo `isLessThan` distToTo) $ do
                    let renderDist dist
                            | dist == zero = "zero"
                            | dist == infinity = "infinity"
                            | otherwise = show dist
                    error $ unwords
                        [ "edge"
                        , showEdge e
                        , "not relaxed."
                        , "distToFrom(" <> show fromInt <> "):", renderDist distToFrom <> ","
                        , "distToTo(" <> show toInt <> "):", renderDist distToTo <> ","
                        , "newDistToTo:", renderDist newDistToTo <> ","
                        , "eMeta:", show (DG.eMeta e)
                        ]
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
                    distToV <- Weight.readArray (distTo state) v
                    distToW <- Weight.readArray (distTo state) w
                    when (calcWeight distToV (DG.eMeta e) /= distToW) $
                        error $ "edge " ++ show e ++ " on shortest path not tight"
