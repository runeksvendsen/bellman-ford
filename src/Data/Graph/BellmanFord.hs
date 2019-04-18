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
, E.WeightedEdge(..)
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
import           Data.Graph.Types.Internal          (Vertex(Vertex))
import qualified Data.Primitive.MutVar              as MV
import qualified Data.Array.MArray                  as Arr
import qualified Data.List.NonEmpty                 as NE
import qualified Control.Monad.State                as S


type BF s g e v = S.StateT (State s g e v) (ST s)

-- |
runBF
    :: DG.Digraph s g e v
    -> (Double -> e -> Double)
    -- | Weight combination function "f".
    --   "f a b" calculates a new distance to a "to" vertex.
    ---  "a" is the distance to the edge's "from vertex",
    --    and "b" is the edge going from "from" to "to". If the value returned by this
    --    function is less than the current distance to "to", the distance to "to" will
    --    be updated.
    --  E.g. for Dijkstra with type parameter "e" equal to "Double",
    --   this function would simply be "(+)".
    -> BF s g e v a
    -> ST s a
runBF graph weightCombine bf = do
    mutState <- initState graph
    let state = State graph weightCombine mutState
    fst <$> S.runStateT bf state

getGraph
    :: BF s g e v (DG.Digraph s g e v)
getGraph = S.gets sGraph

data State s g e v = State
    { sGraph            :: DG.Digraph s g e v
    , sWeightCombine    :: (Double -> e -> Double)
    , sMState           :: MState s g e
    }

-- |
data MState s g e = MState
    { -- | distTo[v] = distance of shortest s->v path
      distTo    :: STUArray s (Vertex g) Double
      -- | edgeTo[v] = last edge on shortest s->v path
    , edgeTo    :: STArray s (Vertex g) (Maybe e)
      -- | onQueue[v] = is v currently on the queue?
    , onQueue   :: STUArray s (Vertex g) Bool
      -- | queue of vertices to relax
    , queue     :: Q.MQueue s (Vertex g)
      -- | number of calls to relax()
    , cost      :: MV.MutVar s Word
      -- | negative cycle (empty list if no such cycle)
    , cycle     :: MV.MutVar s [e]
    }

-- |
bellmanFord
    :: (E.WeightedEdge e v Double, Eq e, Show e)
    => v    -- ^ Source vertex
    -> BF s g e v ()
bellmanFord src = do
    graph <- S.gets sGraph
    -- HACK: make things work for now before algorithm is improved
    mutState <- S.lift $ initState graph
    S.modify (\state -> state { sMState = mutState })
    -- END HACK
    state <- S.gets sMState
    srcVertex <- U.lookupVertex graph src
    S.lift $ Arr.writeArray (distTo state) srcVertex 0.0
    S.lift $ enqueueVertex state srcVertex
    go state
    (`assert` ()) <$> check srcVertex
  where
    go state = do
        vertexM <- S.lift $ dequeueVertex state
        case vertexM of
            Nothing     -> return ()
            Just vertex -> do
                relax vertex
                -- Recurse unless there's a negative cycle
                unlessM hasNegativeCycle (go state)

-- |
pathTo
    :: (E.DirectedEdge e v)
    => v                        -- ^ Target vertex
    -> BF s g e v (Maybe [e])
pathTo target = do
    graph <- S.gets sGraph
    state <- S.gets sMState
    whenM hasNegativeCycle $
        error "Negative cost cycle exists"
    targetVertex <- U.lookupVertex graph target
    pathExists <- S.lift $ hasPathTo state targetVertex
    S.lift $ if pathExists
        then Just <$> go graph state [] targetVertex
        else return Nothing
  where
    go graph state accum toVertex = do
        edgeM <- Arr.readArray (edgeTo state) toVertex
        case edgeM of
            Nothing   -> return accum
            Just edge -> do
                fromNode <- U.lookupVertex graph (E.fromNode edge)
                go graph state (edge : accum) fromNode

hasPathTo
    :: MState s g e
    -> Vertex g
    -> ST s Bool
hasPathTo state target =
    (< (1/0)) <$> Arr.readArray (distTo state) target

-- |
relax
    :: (E.WeightedEdge e v Double, Show e)
    => Vertex g
    -> BF s g e v ()
relax vertex = do
    graph      <- S.gets sGraph
    calcWeight <- S.gets sWeightCombine
    state      <- S.gets sMState
    edgeList   <- DG.outgoingEdges graph vertex
    distToFrom <- S.lift $ Arr.readArray (distTo state) vertex
    vertexCount <- DG.vertexCount graph
    mapM_ (handleEdge graph state calcWeight vertexCount distToFrom) edgeList
  where
    handleEdge graph state calcWeight vertexCount distToFrom edge =
        unlessM hasNegativeCycle $ do
            to <- U.lookupVertex graph (E.toNode edge)
            -- Look up current distance to "to" vertex
            distToTo <- S.lift $ Arr.readArray (distTo state) to
            -- Actual relaxation
            let newToWeight = calcWeight distToFrom edge
            when (distToTo > newToWeight) $ S.lift $ do
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
    :: (E.DirectedEdge e v, Show e)
    => BF s g e v ()
findNegativeCycle = do
    state    <- S.gets sMState
    spEdges  <- S.lift $ Arr.getElems (edgeTo state)
    sptGraph <- DG.fromEdges (catMaybes spEdges)
    S.lift $ C.findCycle sptGraph >>= MV.writeMutVar (cycle state)

hasNegativeCycle
    :: BF s g e v Bool
hasNegativeCycle = do
    state <- S.gets sMState
    fmap (not . null) . MV.readMutVar . cycle $ state

-- | Get negative cycle ('Nothing' in case there's no negative cycle)
negativeCycle
    :: BF s g e v (Maybe (NE.NonEmpty e))
negativeCycle = do
    state    <- S.gets sMState
    edgeList <- MV.readMutVar $ cycle state
    case edgeList of
        []    -> return Nothing
        edges -> return $ Just $ NE.fromList edges

-- | Create initial 'MState'
initState
    :: DG.Digraph s g e v   -- ^ Graph
    -> ST s (MState s g e)   -- ^ Initialized state
initState graph = do
    vertexCount <- Vertex . fromIntegral <$> DG.vertexCount graph
    MState
        <$> Arr.newArray (Vertex 0, vertexCount) (1/0)      -- distTo
        <*> Arr.newArray (Vertex 0, vertexCount) Nothing    -- edgeTo
        <*> Arr.newArray (Vertex 0, vertexCount) False      -- onQueue
        <*> Q.new                                           -- queue
        <*> MV.newMutVar 0                                  -- cost
        <*> MV.newMutVar []                                 -- cycle

-- | Add vertex to queue (helper function)
enqueueVertex
    :: MState s g e
    -> Vertex g
    -> ST s ()
enqueueVertex state vertex = do
    Arr.writeArray (onQueue state) vertex True  -- Mark vertex as being in queue
    Q.enqueue (queue state) vertex              -- Add vertex to queue

-- | Remove vertex from queue (helper function)
dequeueVertex
    :: MState s g e
    -> ST s (Maybe (Vertex g))
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
    :: (E.WeightedEdge e v Double, Eq e, Show e)
    => Vertex g
    -> BF s g e v Bool
check source = do
    graph      <- S.gets sGraph
    calcWeight <- S.gets sWeightCombine
    state      <- S.gets sMState
    ifM hasNegativeCycle
        (checkNegativeCycle state)
        (checkNoCycle state graph calcWeight)
    return True
  where
    checkNegativeCycle state = do
        negativeCycle' <- MV.readMutVar (cycle state)
        -- check that weight of negative cycle is negative
        let weight = sum $ map E.weight negativeCycle'
        when (weight >= 0.0) $
            error $ "negative cycle is non-negative: " ++ show weight
    checkNoCycle state graph calcWeight = S.lift $ do
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
                w <- U.lookupVertex graph (E.toNode e)
                distToV <- Arr.readArray (distTo state) v
                distToW <- Arr.readArray (distTo state) w
                when (calcWeight distToV e < distToW) $
                    error $ "edge " ++ show e ++ " not relaxed"
        -- check that all edges e = v->w on SPT satisfy distTo[w] == distTo[v] + e.weight()
        forM_ vertices $ \w -> do
            edgeM <- Arr.readArray (edgeTo state) w
            case edgeM of
                Nothing -> return ()
                Just e  -> do
                    toVertex <- U.lookupVertex graph (E.toNode e)
                    when (w /= toVertex) $
                        error $ "edgeTo[v].to /= v"
                    v <- U.lookupVertex graph (E.fromNode e)
                    distToV <- Arr.readArray (distTo state) v
                    distToW <- Arr.readArray (distTo state) w
                    when (calcWeight distToV e /= distToW) $
                        error $ "edge " ++ show e ++ " on shortest path not tight"
