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
    -> Double
    -- ^ "Zero-element". With a zero-element of @z@ and a weight-combination
    --  function @weightComb@ then for all @a@: @weightComb z a = a@.
    -- E.g.: equal to 0 if @weightComb@ equals @('+')@ and 1 if @weightComb@ equals @('*')@.
    -> BF s v meta a
    -> ST s a
runBF graph weightCombine zero bf = do
    mutState <- initState graph
    let state = State graph weightCombine zero mutState
    R.runReaderT bf state

getGraph
    :: BF s v meta (DG.Digraph s v meta)
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
      -- | onQueue[v] = is v currently on the queue?
    , onQueue   :: STUArray s Int Bool
      -- | queue of vertices to relax
    , queue     :: Q.MQueue s DG.VertexId
      -- | number of calls to relax()
    , cost      :: MV.MutVar s Word
      -- | negative cycle (empty list if no such cycle)
    , cycle     :: MV.MutVar s [DG.IdxEdge v meta]
    }

-- | Necessary because of floating point rounding errors.
--   Cf. https://stackoverflow.com/a/65051801/700597
epsilon :: Double
epsilon = 1.0e-14

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

-- | NB: has no effect if the source vertex does not exist
bellmanFord
    :: (Ord v, Hashable v, Show v, Show meta, Eq meta)
    => DG.HasWeight meta Double
    => v    -- ^ Source vertex
    -> BF s v meta ()
bellmanFord src = do
    graph <- R.asks sGraph
    state <- R.asks sMState
    srcVertexM <- R.lift (DG.lookupVertex graph src)
    forM_ srcVertexM (initAndGo state)
  where
    initAndGo state srcVertex = do
        resetState state
        zero <- R.asks sZero
        R.lift $ Arr.writeArray (distTo state) (DG.vidInt srcVertex) zero
        R.lift $ enqueueVertex state srcVertex
        go state
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
    :: (Show v, Eq v, Hashable v, Show meta)
    => v                        -- ^ Target vertex
    -> BF s v meta (Maybe [DG.IdxEdge v meta])
pathTo target = do
    graph <- R.asks sGraph
    targetVertexM <- fmap DG.vidInt <$> R.lift (DG.lookupVertex graph target)
    maybe (return Nothing) (findPath graph) targetVertexM
  where
    findPath graph targetVertex = do
        negativeCycle >>= maybe (return ()) failNegativeCycle -- Check for negative cycle
        state <- R.asks sMState
        pathExists <- R.lift $ hasPathTo state targetVertex
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
    :: MState s g e
    -> Int
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
    distToFrom <- R.lift $ Arr.readArray (distTo state) (DG.vidInt vertex)
    vertexCount <- R.lift $ DG.vertexCount graph
    mapM_ (handleEdge state calcWeight vertexCount distToFrom) edgeList
  where
    handleEdge state calcWeight vertexCount distToFrom edge =
        unlessM hasNegativeCycle $ do
            let to = DG.eToIdx edge
                toInt = DG.vidInt to
            -- Look up current distance to "to" vertex
            distToTo <- R.lift $ Arr.readArray (distTo state) toInt
            -- Actual relaxation
            let newToWeight = calcWeight distToFrom (DG.eMeta edge)
            when (distToTo > newToWeight + epsilon) $ R.lift $ do
                Arr.writeArray (distTo state) toInt newToWeight
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
    => BF s v meta ()
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
    vertexCount <- fromIntegral <$> DG.vertexCount graph
    MState
        <$> Arr.newArray (0, vertexCount) (1/0)      -- distTo
        <*> Arr.newArray (0, vertexCount) Nothing    -- edgeTo
        <*> Arr.newArray (0, vertexCount) False      -- onQueue
        <*> Q.new                                           -- queue
        <*> MV.newMutVar 0                                  -- cost
        <*> MV.newMutVar []                                 -- cycle

-- | Add vertex to queue (helper function)
enqueueVertex
    :: MState s g e
    -> DG.VertexId
    -> ST s ()
enqueueVertex state vertex = do
    Arr.writeArray (onQueue state) (DG.vidInt vertex) True  -- Mark vertex as being in queue
    Q.enqueue (queue state) vertex              -- Add vertex to queue

-- | Remove vertex from queue (helper function)
dequeueVertex
    :: MState s g e
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
    :: (Eq meta, Show meta, Show v, DG.HasWeight meta Double, Eq v)
    => Int
    -> BF s v meta Bool
check source = do
    graph      <- R.asks sGraph
    zero       <- R.asks sZero
    calcWeight <- R.asks sWeightCombine
    state      <- R.asks sMState
    ifM hasNegativeCycle
        (checkNegativeCycle zero calcWeight state)
        (checkNoCycle state graph zero calcWeight)
    return True
  where
    checkNegativeCycle zero calcWeight state = do
        negativeCycle' <- MV.readMutVar (cycle state)
        -- check that weight of negative cycle is negative
        let weight = foldr (flip calcWeight . DG.eMeta) zero negativeCycle'
        when (weight >= zero) $
            error $ unlines [ "negative cycle is non-negative"
                            , printf "weight: %s" (show weight)
                            , printf "edges: %s" (show negativeCycle')
                            ]
    checkNoCycle state graph zero calcWeight = R.lift $ do
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
