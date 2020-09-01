{-# LANGUAGE FlexibleContexts #-}
module Data.Graph.BellmanFord
( -- * Monad
  runBF
, BF
  -- * Algorithm
, bellmanFord
  -- * Queries
, pathTo
, negativeCycle
  -- * Types
, E.DirectedEdge(..)
  -- * Extras
, getGraph
)
where

import           Prelude                            hiding (cycle)
import           Data.Graph.Prelude
import qualified Data.Graph.Util                    as U
import qualified Data.Graph.Digraph                 as DG
import qualified Data.Graph.Edge                    as E
import qualified Data.Graph.Cycle                   as C
import           Control.Monad.ST                   (ST)
import           Data.Array.ST                      (STArray, STUArray)
import qualified Data.Queue                         as Q
import qualified Data.Primitive.MutVar              as MV
import qualified Data.Array.MArray                  as Arr
import qualified Data.List.NonEmpty                 as NE
import qualified Control.Monad.Reader               as R
import           Data.Ix                            (range)


type BF s v meta = R.ReaderT (State s v meta) (ST s)

-- |
runBF
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
    -> BF s v meta a
    -> ST s a
runBF graph weightCombine bf = do
    mutState <- initState graph
    let state = State graph weightCombine mutState
    R.runReaderT bf state

getGraph
    :: BF s v meta (DG.Digraph s v meta)
getGraph = R.asks sGraph

data State s v meta = State
    { sGraph            :: DG.Digraph s v meta
    , sWeightCombine    :: (Double -> meta -> Double)
    , sMState           :: MState s v meta
    }

-- |
data MState s v meta = MState
    { -- | distTo[v] = distance of shortest s->v path
      distTo    :: STUArray s (DG.VertexId) Double
      -- | edgeTo[v] = last edge on shortest s->v path
    , edgeTo    :: STArray s (DG.VertexId) (Maybe (DG.IdxEdge v meta))
      -- | onQueue[v] = is v currently on the queue?
    , onQueue   :: STUArray s (DG.VertexId) Bool
      -- | queue of vertices to relax
    , queue     :: Q.MQueue s (DG.VertexId)
      -- | number of calls to relax()
    , cost      :: MV.MutVar s Word
      -- | negative cycle (empty list if no such cycle)
    , cycle     :: MV.MutVar s [DG.IdxEdge v meta]
    }

-- | Reset state in 'MState' so that it's the same as returned by 'initState'
resetState
    :: MState s g e
    -> BF s v meta ()
resetState mutState = R.lift $ do
    fillArray (distTo mutState) (1/0)
    fillArray (edgeTo mutState) Nothing
    fillArray (onQueue mutState) False
    emptyQueue (queue mutState)
    MV.atomicModifyMutVar' (cost mutState) (const (0, ()))
    MV.atomicModifyMutVar' (cycle mutState) (const ([], ()))
  where
    emptyQueue
        :: Q.MQueue s (DG.VertexId)
        -> ST s ()
    emptyQueue queue' = do
        let go = maybe (return ()) (\_ -> Q.dequeue queue' >>= go)
        Q.dequeue queue' >>= go
    fillArray
        :: Arr.MArray a e (ST s)
        => a (DG.VertexId) e
        -> e
        -> (ST s) ()
    fillArray arr value = do
        indices <- range <$> Arr.getBounds arr
        forM_ indices (\idx -> Arr.writeArray arr idx value)

-- |
bellmanFord
    :: (Ord v, Hashable v, Show v, Show meta, Eq meta)
    => DG.HasWeight meta Double
    => v    -- ^ Source vertex
    -> BF s v meta ()
bellmanFord src = do
    graph <- R.asks sGraph
    state <- R.asks sMState
    resetState state    -- HACK: make things work for now before algorithm is improved
    srcVertex <- R.lift $ U.lookupVertex graph src
    R.lift $ Arr.writeArray (distTo state) srcVertex 0.0
    R.lift $ enqueueVertex state srcVertex
    go state
    (`assert` ()) <$> check srcVertex
  where
    go state = do
        vertexM <- R.lift $ dequeueVertex state
        case vertexM of
            Nothing     -> return ()
            Just vertex -> do
                relax vertex
                -- Recurse unless there's a negative cycle
                unlessM hasNegativeCycle (go state)

-- |
pathTo
    :: (Show v, Eq v, Hashable v, Show meta)
    => v                        -- ^ Target vertex
    -> BF s v meta (Maybe [DG.IdxEdge v meta])
pathTo target = do
    graph <- R.asks sGraph
    state <- R.asks sMState
    -- Check for negative cycle
    negativeCycle >>= maybe (return ()) failNegativeCycle
    targetVertex <- R.lift $ U.lookupVertex graph target
    pathExists <- R.lift $ hasPathTo state targetVertex
    R.lift $ if pathExists
        then Just <$> go graph state [] targetVertex
        else return Nothing
  where
    failNegativeCycle cycle' =
        error $ "Negative-cost cycle exists (target=" ++ show target ++ "): " ++ show cycle'
    go graph state accum toVertex = do
        edgeM <- Arr.readArray (edgeTo state) toVertex
        case edgeM of
            Nothing   -> return accum
            Just edge -> do
                let fromNode = DG.eFromIdx edge
                go graph state (edge : accum) fromNode

hasPathTo
    :: MState s g e
    -> DG.VertexId
    -> ST s Bool
hasPathTo state target =
    (< (1/0)) <$> Arr.readArray (distTo state) target

-- |
relax
    :: (Show v, Ord v, Hashable v, Show meta)
    => DG.VertexId
    -> BF s v meta ()
relax vertex = do
    graph      <- R.asks sGraph
    calcWeight <- R.asks sWeightCombine
    state      <- R.asks sMState
    edgeList   <- R.lift $ DG.outgoingEdges graph vertex
    distToFrom <- R.lift $ Arr.readArray (distTo state) vertex
    vertexCount <- R.lift $ DG.vertexCount graph
    mapM_ (handleEdge state calcWeight vertexCount distToFrom) edgeList
  where
    handleEdge state calcWeight vertexCount distToFrom edge =
        unlessM hasNegativeCycle $ do
            let to = DG.eToIdx edge
            -- Look up current distance to "to" vertex
            distToTo <- R.lift $ Arr.readArray (distTo state) to
            -- Actual relaxation
            let newToWeight = calcWeight distToFrom (DG.eMeta edge)
            when (distToTo > newToWeight) $ R.lift $ do
                Arr.writeArray (distTo state) to newToWeight
                Arr.writeArray (edgeTo state) to (Just edge)
                unlessM (Arr.readArray (onQueue state) to) $
                    enqueueVertex state to
            -- Update cost (number of calls to "relax")
            newCost <- MV.atomicModifyMutVar' (cost state)
                (\cost' -> let newCost = cost' + 1 in (newCost, newCost))
            when (newCost `mod` vertexCount == 0) $
                findNegativeCycle

findNegativeCycle
    :: (Ord v, Show v, Show meta, Hashable v)
    => BF s v meta ()
findNegativeCycle = do
    state    <- R.asks sMState
    spEdges  <- R.lift $ Arr.getElems (edgeTo state)
    sptGraph <- R.lift $ DG.fromEdges (catMaybes spEdges)
    R.lift $ C.findCycle sptGraph >>= MV.writeMutVar (cycle state)

hasNegativeCycle
    :: BF s v meta Bool
hasNegativeCycle = do
    state <- R.asks sMState
    fmap (not . null) . MV.readMutVar . cycle $ state

-- | Get negative cycle ('Nothing' in case there's no negative cycle)
negativeCycle
    :: BF s v meta (Maybe (NE.NonEmpty (DG.IdxEdge v meta)))
negativeCycle = do
    state    <- R.asks sMState
    edgeList <- MV.readMutVar $ cycle state
    case edgeList of
        []    -> return Nothing
        edges -> return $ Just $ NE.fromList edges

-- | Create initial 'MState'
initState
    :: DG.Digraph s v meta   -- ^ Graph
    -> ST s (MState s g e)   -- ^ Initialized state
initState graph = do
    vertexCount <- DG.VertexId . fromIntegral <$> DG.vertexCount graph
    MState
        <$> Arr.newArray (DG.VertexId 0, vertexCount) (1/0)      -- distTo
        <*> Arr.newArray (DG.VertexId 0, vertexCount) Nothing    -- edgeTo
        <*> Arr.newArray (DG.VertexId 0, vertexCount) False      -- onQueue
        <*> Q.new                                           -- queue
        <*> MV.newMutVar 0                                  -- cost
        <*> MV.newMutVar []                                 -- cycle

-- | Add vertex to queue (helper function)
enqueueVertex
    :: MState s g e
    -> DG.VertexId
    -> ST s ()
enqueueVertex state vertex = do
    Arr.writeArray (onQueue state) vertex True  -- Mark vertex as being in queue
    Q.enqueue (queue state) vertex              -- Add vertex to queue

-- | Remove vertex from queue (helper function)
dequeueVertex
    :: MState s g e
    -> ST s (Maybe (DG.VertexId))
dequeueVertex state = do
    -- Remove vertex from queue
    vertexM <- Q.dequeue (queue state)
    -- Mark vertex as not being in queue
    maybe (return ()) (\vertex -> Arr.writeArray (onQueue state) vertex False) vertexM
    return vertexM

-- | check optimality conditions: either
-- (i) there exists a negative cycle reachable from s
--     or
-- (ii)  for all edges e = v->w:            distTo[w] <= distTo[v] + e.weight()
-- (ii') for all edges e = v->w on the SPT: distTo[w] == distTo[v] + e.weight()
check
    :: (Eq meta, Show meta, Show v, DG.HasWeight meta Double, Eq v)
    => DG.VertexId
    -> BF s v meta Bool
check source = do
    graph      <- R.asks sGraph
    calcWeight <- R.asks sWeightCombine
    state      <- R.asks sMState
    ifM hasNegativeCycle
        (checkNegativeCycle state)
        (checkNoCycle state graph calcWeight)
    return True
  where
    checkNegativeCycle state = do
        negativeCycle' <- MV.readMutVar (cycle state)
        -- check that weight of negative cycle is negative
        let weight = sum $ map (DG.weight . DG.eMeta) negativeCycle'
        when (weight >= 0.0) $
            error $ unlines [ "negative cycle is non-negative"
                            , printf "weight: %s" (show weight)
                            , printf "edges: %s" (show negativeCycle')
                            ]
    checkNoCycle state graph calcWeight = R.lift $ do
        -- check that distTo[v] and edgeTo[v] are consistent
        whenM ((/= 0.0) <$> Arr.readArray (distTo state) source) $
            error "distTo source /= 0"
        whenM ((/= Nothing) <$> Arr.readArray (edgeTo state) source) $
            error "edgeTo source /= null"
        -- check that: edgeTo[v] == null implies distTo[v] == infinity and vice versa
        vertices <- DG.vertices graph
        forM_ vertices $ \v ->
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
                distToV <- Arr.readArray (distTo state) v
                distToW <- Arr.readArray (distTo state) w
                when (calcWeight distToV (DG.eMeta e) < distToW) $
                    error $ "edge " ++ show e ++ " not relaxed"
        -- check that all edges e = v->w on SPT satisfy distTo[w] == distTo[v] + e.weight()
        forM_ vertices $ \w -> do
            edgeM <- Arr.readArray (edgeTo state) w
            case edgeM of
                Nothing -> return ()
                Just e  -> do
                    let toVertex = DG.eToIdx e
                    when (w /= toVertex) $
                        error $ "edgeTo[v].to /= v"
                    let v = DG.eFromIdx e
                    distToV <- Arr.readArray (distTo state) v
                    distToW <- Arr.readArray (distTo state) w
                    when (calcWeight distToV (DG.eMeta e) /= distToW) $
                        error $ "edge " ++ show e ++ " on shortest path not tight"
