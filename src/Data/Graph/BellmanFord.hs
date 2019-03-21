{-# LANGUAGE FlexibleContexts #-}
module Data.Graph.BellmanFord
( -- * Algorithm
  bellmanFord
  -- * Types
, State
, E.DirectedEdge
  -- * Test
, check
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


-- |
data State s g e = State
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

bellmanFord
    :: (E.WeightedEdge e v Double, Eq e, Show e)
    => DG.Digraph s g e v
    -> Vertex g
    -> ST s (State s g e)
bellmanFord graph src = do
    state <- initState graph src
    go state
    (`assert` ()) <$> check graph state src
    return state
  where
    go state = do
        vertexM <- dequeueVertex state
        case vertexM of
            Nothing     -> return ()
            Just vertex -> do
                relax graph state vertex
                -- Recurse unless there's a negative cycle
                unlessM (hasNegativeCycle state) (go state)

relax
    :: (E.WeightedEdge e v Double, Show e)
    => DG.Digraph s g e v
    -> State s g e
    -> Vertex g
    -> ST s ()
relax graph state vertex = do
    edgeList <- DG.outgoingEdges graph vertex
    distToFrom <- Arr.readArray (distTo state) vertex
    vertexCount <- DG.vertexCount graph
    mapM_ (handleEdge vertexCount distToFrom) edgeList
  where
    handleEdge vertexCount distToFrom edge =
        unlessM (hasNegativeCycle state) $ do
            to <- U.lookupVertex graph (E.toNode edge)
            -- Look up current distance to "to" vertex
            distToTo <- Arr.readArray (distTo state) to
            -- Actual relaxation
            when (distToTo > distToFrom + E.weight edge) $ do
                Arr.writeArray (distTo state) to (distToFrom + E.weight edge)
                Arr.writeArray (edgeTo state) to (Just edge)
                whenM (Arr.readArray (onQueue state) to) $
                    enqueueVertex state to
            -- Update cost (number of calls to "relax")
            newCost <- MV.atomicModifyMutVar' (cost state)
                (\cost' -> let newCost = cost' + 1 in (newCost, newCost))
            when (newCost `mod` vertexCount == 0) $ do
                findNegativeCycle state

findNegativeCycle
    :: (E.DirectedEdge e v, Show e)
    => State s g e
    -> ST s ()
findNegativeCycle state = do
    spEdges  <- Arr.getElems (edgeTo state)
    sptGraph <- DG.fromEdges (catMaybes spEdges)
    C.findCycle sptGraph >>= MV.writeMutVar (cycle state)

hasNegativeCycle
    :: State s g e
    -> ST s Bool
hasNegativeCycle = fmap (not . null) . MV.readMutVar . cycle

-- | Create initial 'State'
initState
    :: DG.Digraph s g e v   -- ^ Graph
    -> Vertex g             -- ^ Source vertex
    -> ST s (State s g e)   -- ^ Initialized state
initState graph src = do
    vertexCount <- Vertex . fromIntegral <$> DG.vertexCount graph
    state <- State
        <$> Arr.newArray (Vertex 0, vertexCount) (1/0)      -- distTo
        <*> Arr.newArray (Vertex 0, vertexCount) Nothing    -- edgeTo
        <*> Arr.newArray (Vertex 0, vertexCount) False      -- onQueue
        <*> Q.new                                           -- queue
        <*> MV.newMutVar 0                                  -- cost
        <*> MV.newMutVar []                                 -- cycle
    Arr.writeArray (distTo state) src 0.0
    enqueueVertex state src
    return state

-- | Add vertex to queue (helper function)
enqueueVertex
    :: State s g e
    -> Vertex g
    -> ST s ()
enqueueVertex state vertex = do
    Arr.writeArray (onQueue state) vertex True    -- Mark vertex as being in queue
    Q.enqueue (queue state) vertex            -- Add vertex to queue

-- | Remove vertex from queue (helper function)
dequeueVertex
    :: State s g e
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
    => DG.Digraph s g e v
    -> State s g e
    -> Vertex g
    -> ST s Bool
check graph state source = do
    ifM (hasNegativeCycle state) checkNegativeCycle checkNoCycle
    return True
  where
    checkNegativeCycle = do
        negativeCycle <- MV.readMutVar (cycle state)
        -- check that weight of negative cycle is negative
        let weight = sum $ map E.weight negativeCycle
        when (weight >= 0.0) $
            error $ "negative cycle is non-negative: " ++ show weight
    checkNoCycle = do
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
                when (distToV + E.weight e < distToW) $
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
                    when (distToV + E.weight e /= distToW) $
                        error $ "edge " ++ show e ++ " on shortest path not tight"
