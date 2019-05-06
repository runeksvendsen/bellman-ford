{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Graph.BellmanFord
( -- * Monad
  runBF
, runBFOptimized
, BF
  -- * Queries
, pathTo
, negativeCycle
  -- * Types
, E.DirectedEdge(..)
, E.WeightedEdge(..)
  -- * Classes
, RelaxUpdate(..)
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
import           Control.Monad.Primitive            (PrimMonad(..), PrimState)
import           Control.Monad.ST                   (ST)
import           Data.Array.ST                      (STArray, STUArray)
import qualified Data.Queue                         as Q
import           Data.Graph.Types.Internal          (Vertex(Vertex))
import qualified Data.Primitive.MutVar              as MV
import qualified Data.Array.MArray                  as Arr
import qualified Data.List.NonEmpty                 as NE
import qualified Control.Monad.Reader               as R
import           Data.Ix                            (range)



newtype Arbitrage (src :: Symbol) a = Arbitrage [NE.NonEmpty a]
newtype MatchResult (src :: Symbol) a = MatchResult [NE.NonEmpty a]


newAlgorithm
    :: forall src s g e v. KnownSymbol src
    => BF s g e v (Arbitrage src e, MatchResult src e)
newAlgorithm targetVertex = do
    graph <- getGraph
    -- Arbitrages
    let goArbitrages accum = do
        bellmanFord (symbolVal (Proxy :: Proxy src))
        negativeCycleM <- negativeCycle
        case negativeCycleM of
            Just edges -> do
                forM_ edges (DG.removeEdge graph)

                goArbitrages $ edges : accum
            Nothing -> return $ Arbitrage accum
    arbitrage <- goArbitrages []
    -- MatchResult
    let goMatch accum = do
        matchedOrdersM <- pathTo targetVertex
        case matchedOrdersM of
            Just edges -> do
                forM_ edges (DG.removeEdge graph)
                goMatch $ edges : accum
            Nothing -> return $ MatchResult accum
    matchResult <- goMatch []
    return (arbitrage, matchResult)



type BFUpdateAll s g e v     = BFUpdate s g e v 'RelaxAll
type BFUpdateChanged s g e v = BFUpdate s g e v 'RelaxChanged
type BF s g e v = BFUpdateAll s g e v

newtype BFUpdate s g e v (mode :: UpdateMode) a =
    BFUpdate { getBF :: R.ReaderT (State s g e v) (ST s) a }
        deriving ( Functor
                 , Applicative
                 , Monad
                 , R.MonadReader (State s g e v)
                 )

instance PrimMonad (BFUpdate s g e v mode) where
  type PrimState (BFUpdate s g e v mode) = s
  primitive = BFUpdate . primitive
  {-# INLINE primitive #-}

liftST
    :: ST s a
    -> BFUpdate s g e v mode a
liftST = BFUpdate . R.lift

-- | How are relaxed vertices updated when one or more graph edges
--    are replaced/removed?
data UpdateMode
    = RelaxAll
    | RelaxChanged

class E.DirectedEdge e v => RelaxUpdate m e v where
    updateEdge :: e -> m ()
    removeEdge :: e -> m ()

instance (E.WeightedEdge e v Double, Show e) => RelaxUpdate (BFUpdate s g e v 'RelaxAll) e v where
    updateEdge edge = error "STUB"
    removeEdge edge = do
        state <- R.asks sMState
        vertices <- liftST $ range <$> Arr.getBounds (distTo state)
        liftST $ resetMState state vertices

instance (E.WeightedEdge e v Double, Show e) => RelaxUpdate (BFUpdate s g e v 'RelaxChanged) e v where
    updateEdge edge = do
        -- Update edge in graph
        graph <- R.asks sGraph
        (from, to) <- DG.insertEdge graph edge
        state <- R.asks sMState
        -- BEGIN assertion
        existingEdge <- fromMaybe (error $ "Missing 'edgeTo' edge for: " ++ show edge) <$>
                            liftST (Arr.readArray (edgeTo state) to)
        assert (  E.fromNode existingEdge == E.fromNode edge
               && E.toNode existingEdge == E.toNode edge
               ) (return ())
        -- END assertion
        when (E.weight existingEdge == E.weight edge) $
            liftST $ Arr.writeArray (edgeTo state) to (Just edge)
        -- TODO: how to handle a change in a cycle?
        MV.writeMutVar (cycle state) []
        -- Relax "from" vertex
        unless (E.weight existingEdge == E.weight edge) $
            relax from

    removeEdge edge = do
        -- Remove edge from graph
        graph <- R.asks sGraph
        (from, to) <- DG.removeEdge graph edge
        -- Make "to" vertex unrelaxed
        state <- R.asks sMState
        liftST $ Arr.writeArray (distTo state) to (1/0)
        liftST $ Arr.writeArray (edgeTo state) to Nothing
        MV.writeMutVar (cycle state) []
        -- Relax "from" vertex
        relax from

-- |
runBF
    :: ( E.WeightedEdge e v Double
       , Eq e
       , Show e
       )
    => DG.Digraph s g e v
    -> (Double -> e -> Double)
    -- | Weight combination function "f".
    --   "f a b" calculates a new distance to a "to" vertex.
    ---  "a" is the distance to the edge's "from vertex",
    --    and "b" is the edge going from "from" to "to". If the value returned by this
    --    function is less than the current distance to "to", the distance to "to" will
    --    be updated.
    --  E.g. for Dijkstra with type parameter "e" equal to "Double",
    --   this function would simply be "(+)".
    -> v    -- ^ Source vertex
    -> BFUpdate s g e v 'RelaxAll a
    -> ST s a
runBF = runBFMulti

-- |
runBFOptimized
    :: ( E.WeightedEdge e v Double
       , Eq e
       , Show e
       )
    => DG.Digraph s g e v
    -> (Double -> e -> Double)
    -> v
    -> BFUpdate s g e v 'RelaxChanged a
    -> ST s a
runBFOptimized = runBFMulti

-- |
runBFMulti
    :: ( RelaxUpdate (BFUpdate s g e v mode) e v
       , E.WeightedEdge e v Double
       , Eq e
       , Show e
       )
    => DG.Digraph s g e v
    -> (Double -> e -> Double)
    -> v
    -> BFUpdate s g e v mode a
    -> ST s a
runBFMulti graph weightCombine src bf = do
    -- Initialize mutable state
    mutState <- createMState graph
    -- resetMState mutState allVertices
    srcVertex <- U.lookupVertex graph src
    let state = State graph weightCombine mutState srcVertex
    R.runReaderT (getBF $ bellmanFord >> bf) state

getGraph
    :: BFUpdate s g e v mode (DG.Digraph s g e v)
getGraph = R.asks sGraph

data State s g e v = State
    { sGraph            :: DG.Digraph s g e v
    , sWeightCombine    :: (Double -> e -> Double)
    , sMState           :: MState s g e
    , sSrc              :: Vertex g
    }

-- |
data MState s g e = MState
    { -- | distTo[v] = distance of shortest s->v path
      distTo    :: STUArray s (Vertex g) Double
      -- | edgeTo[v] = last edge on shortest s->v path
    , edgeTo    :: STArray s (Vertex g) (Maybe e)
      -- | onQueue[v] = is v currently on the queue?
    , onQueue   :: STUArray s (Vertex g) Bool   -- TODO: merge into "queue" implementation below
      -- | queue of vertices to relax
    , queue     :: Q.MQueue s (Vertex g)
      -- | number of calls to relax()
    , cost      :: MV.MutVar s Word
      -- | negative cycle (empty list if no such cycle)
    , cycle     :: MV.MutVar s [e]
      -- | vertices whose state is not valid until "bellmanFord" has been run again
    , dirty     :: MV.MutVar s [Vertex g]
    -- TODO: remove
    }

-- | Reset state in 'MState' for the given vertices
resetMState
    :: forall s g e.
       MState s g e
    -> [Vertex g]
    -> ST s ()
resetMState mutState indices = do
    fillArray (distTo mutState) (1/0)
    fillArray (edgeTo mutState) Nothing
    fillArray (onQueue mutState) False
    MV.atomicModifyMutVar' (cycle mutState) (const ([], ()))
  where
    fillArray
        :: Arr.MArray a elem (ST s)
        => a (Vertex g) elem
        -> elem
        -> (ST s) ()
    fillArray arr value =
        forM_ indices (\idx -> Arr.writeArray arr idx value)

-- |
bellmanFord
    :: ( E.WeightedEdge e v Double
       , Eq e
       , Show e
       )
    => BFUpdate s g e v mode ()
bellmanFord = do
    state <- R.asks sMState
    srcVertex <- R.asks sSrc
    liftST $ Arr.writeArray (distTo state) srcVertex 0.0
    liftST $ enqueueVertex state srcVertex
    go state
    (`assert` ()) <$> check srcVertex
  where
    go state = do
        vertexM <- liftST $ dequeueVertex state
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
    -> BFUpdate s g e v mode (Maybe [e])
pathTo target = do
    graph <- R.asks sGraph
    state <- R.asks sMState
    whenM hasNegativeCycle $
        error "Negative cost cycle exists"
    targetVertex <- U.lookupVertex graph target
    pathExists <- liftST $ hasPathTo state targetVertex
    liftST $ if pathExists
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
    -> BFUpdate s g e v mode ()
relax vertex = do
    graph      <- R.asks sGraph
    calcWeight <- R.asks sWeightCombine
    state      <- R.asks sMState
    edgeList   <- DG.outgoingEdges graph vertex
    distToFrom <- liftST $ Arr.readArray (distTo state) vertex
    vertexCount <- DG.vertexCount graph
    mapM_ (handleEdge graph state calcWeight vertexCount distToFrom) edgeList
  where
    handleEdge graph state calcWeight vertexCount distToFrom edge =
        unlessM hasNegativeCycle $ do
            to <- U.lookupVertex graph (E.toNode edge)
            -- Look up current distance to "to" vertex
            distToTo <- liftST $ Arr.readArray (distTo state) to
            -- Actual relaxation
            let newToWeight = calcWeight distToFrom edge
            when (distToTo > newToWeight) $ liftST $ do
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
    => BFUpdate s g e v mode ()
findNegativeCycle = do
    state    <- R.asks sMState
    spEdges  <- liftST $ Arr.getElems (edgeTo state)
    sptGraph <- DG.fromEdges (catMaybes spEdges)
    liftST $ C.findCycle sptGraph >>= MV.writeMutVar (cycle state)

hasNegativeCycle
    :: BFUpdate s g e v mode Bool
hasNegativeCycle = do
    state <- R.asks sMState
    fmap (not . null) . MV.readMutVar . cycle $ state

-- | Get negative cycle ('Nothing' in case there's no negative cycle)
negativeCycle
    :: BFUpdate s g e v mode (Maybe (NE.NonEmpty e))
negativeCycle = do
    state    <- R.asks sMState
    edgeList <- MV.readMutVar $ cycle state
    case edgeList of
        []    -> return Nothing
        edges -> return $ Just $ NE.fromList edges

-- | Create initial 'MState'
createMState
    :: DG.Digraph s g e v   -- ^ Graph
    -> ST s (MState s g e)  -- ^ Initial state
createMState graph = do
    vertexCount <- Vertex . fromIntegral <$> DG.vertexCount graph
    let arrayRange = (Vertex 0, vertexCount)
    let allVertices = range arrayRange
    MState
        <$> Arr.newArray_ arrayRange    -- distTo
        <*> Arr.newArray_ arrayRange    -- edgeTo
        <*> Arr.newArray_ arrayRange    -- onQueue
        <*> Q.new                       -- queue
        <*> MV.newMutVar 0              -- cost
        <*> MV.newMutVar []             -- cycle
        <*> MV.newMutVar allVertices    -- dirty

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
    -> BFUpdate s g e v mode Bool
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
        let weight = sum $ map E.weight negativeCycle'
        when (weight >= 0.0) $
            error $ "negative cycle is non-negative: " ++ show weight
    checkNoCycle state graph calcWeight = liftST $ do
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
