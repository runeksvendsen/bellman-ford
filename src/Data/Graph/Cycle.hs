{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Graph.Cycle
( findCycle
)
where

import           Prelude                            hiding (cycle)
import           Data.Graph.Prelude
import qualified Data.Graph.Util                    as U
import qualified Data.Graph.Digraph                 as DG
import qualified Data.Graph.Edge                    as E
import           Control.Monad.ST                   (ST)
import qualified Data.Array.ST                      as ST
import qualified Data.Stack                         as S
import           Data.Graph.Types.Internal          (Vertex(Vertex))
import qualified Data.Primitive.MutVar              as MV
import qualified Data.Vector.Fusion.Stream.Monadic  as S
import qualified Data.Array.MArray                  as Arr
import qualified Data.List.NonEmpty                 as NE


-- |
data State s g e = State
    { -- | marked[v] = has vertex v been marked?
      marked    :: ST.STUArray s (Vertex g) Bool
      -- | edgeTo[v] = previous edge on path to v
    , edgeTo    :: ST.STArray s (Vertex g) (Maybe e)
      -- | onStack[v] = is vertex on the stack?
    , onStack   :: ST.STUArray s (Vertex g) Bool
      -- | directed cycle (empty list = no cycle)
    , cycle     :: MV.MutVar s [e]
    }

-- | Return cycle (empty list if no cycle exists)
findCycle
    :: (E.DirectedEdge e v)
    => DG.Digraph s g e v
    -> ST s [e]
findCycle graph = do
    state <- initState graph
    vertices <- DG.vertices graph
    forM_ vertices $ \vertex ->
        unlessM (Arr.readArray (marked state) vertex) $ dfs graph state vertex
    MV.readMutVar (cycle state)
    -- TODO: assert check()

dfs :: (E.DirectedEdge e v)
    => DG.Digraph s g e v   -- ^ Graph
    -> State s g e          -- ^ State
    -> Vertex g             -- ^ Start vertex
    -> ST s ()
dfs graph state vertex = do
    Arr.writeArray (onStack state) vertex True
    Arr.writeArray (marked state) vertex True
    -- Iterate over outgoing edges
    DG.outgoingEdges graph vertex >>= S.mapM_ handleEdge
    Arr.writeArray (onStack state) vertex False
  where
    handleEdge edge =
        unlessM (hasCycle state) $ do
            w <- U.lookupVertex graph (E.toNode edge)
            wMarked <- Arr.readArray (marked state) w
            if not wMarked
                then do
                    -- found new vertex, so recur
                    Arr.writeArray (edgeTo state) w (Just edge)
                    dfs graph state w
                else
                    -- trace back directed cycle
                    whenM (Arr.readArray (onStack state) w) $
                        traceBackCycle graph state w edge
                            >>= MV.writeMutVar (cycle state)

-- | Trace back a cycle by starting from the last edge in the cycle,
--    and going back until we reach an edge with a "from"-vertex that
--    equals the given vertex.
traceBackCycle
    :: forall s g e v.
       (Hashable v, E.DirectedEdge e v)
    => DG.Digraph s g e v   -- ^ Graph
    -> State s g e          -- ^ State
    -> Vertex g             -- ^ Vertex where cycle ends (and starts)
    -> e                    -- ^ The last edge of the cycle
    -> ST s [e]
traceBackCycle graph state startVertex lastEdge =
    go (lastEdge NE.:| [])
  where
    go :: NE.NonEmpty e -> ST s [e]
    go accum@(edge NE.:| _) = do
        fromVertex <- U.lookupVertex graph (E.fromNode edge)
        if fromVertex /= startVertex
            then do
                newEdgeM <- Arr.readArray (edgeTo state) fromVertex
                let newEdge = fromMaybe (error missingEdgeError) newEdgeM
                    missingEdgeError = "BUG: edgeTo[edge.from()] not present for edge"
                go $ newEdge `NE.cons` accum
            else return $ NE.toList accum

hasCycle
    :: State s g e
    -> ST s Bool
hasCycle = fmap (not . null) . MV.readMutVar . cycle

-- | Create initial 'State'
initState
    :: DG.Digraph s g e v   -- ^ Graph
    -> ST s (State s g e)   -- ^ Initialized state
initState graph = do
    vertexCount <- Vertex . fromIntegral <$> DG.vertexCount graph
    state <- State
        <$> Arr.newArray (Vertex 0, vertexCount) False      -- marked
        <*> Arr.newArray (Vertex 0, vertexCount) Nothing    -- edgeTo
        <*> Arr.newArray (Vertex 0, vertexCount) False      -- onStack
        <*> MV.newMutVar []                             -- cycle
    return state

-- certify that digraph is either acyclic or has a directed cycle
check state = do
    whenM (hasCycle state) $ do
        return ()
        -- verify cycle:
    return True