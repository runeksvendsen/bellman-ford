{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Graph.Cycle
( -- * Library
  findCycle
  -- * Testing
, verifyCycle
)
where

import           Prelude                            hiding (cycle)
import           Data.Graph.Prelude
import qualified Data.Graph.Digraph                 as DG
import qualified Data.Graph.Edge                    as E
import qualified Data.Array.ST                      as ST
import qualified Data.Primitive.MutVar              as MV
import qualified Data.Array.MArray                  as Arr
import qualified Data.List.NonEmpty                 as NE
import           Data.List                          (foldl')


-- |
data State s g v meta = State
    { -- | marked[v] = has vertex v been marked?
      marked    :: ST.STUArray s Int Bool
      -- | edgeTo[v] = previous edge on path to v
    , edgeTo    :: ST.STArray s Int (Maybe (DG.IdxEdge v meta))
      -- | onStack[v] = is vertex on the stack?
    , onStack   :: ST.STUArray s Int Bool
      -- | directed cycle (empty list = no cycle)
    , cycle     :: MV.MutVar s [DG.IdxEdge v meta]
    }

-- | Return cycle (empty list if no cycle exists)
findCycle
    :: (Show v, Eq v, Hashable v, Show meta)
    => DG.Digraph s v meta
    -> ST s [DG.IdxEdge v meta]
findCycle graph = do
    state <- initState graph
    vertices <- DG.vertices graph
    forM_ vertices $ \vertex ->
        unlessM (Arr.readArray (marked state) (DG.vidInt vertex)) $ dfs graph state vertex
    (`assert` ()) <$> check state
    MV.readMutVar (cycle state)

dfs :: (Show v, Hashable v)
    => DG.Digraph s v meta   -- ^ Graph
    -> State s g v meta          -- ^ State
    -> DG.VertexId             -- ^ Start vertex
    -> ST s ()
dfs graph state vertex = do
    Arr.writeArray (onStack state) (DG.vidInt vertex) True
    Arr.writeArray (marked state) (DG.vidInt vertex) True
    -- Iterate over outgoing edges
    DG.outgoingEdges graph vertex >>= mapM_ handleEdge
    Arr.writeArray (onStack state) (DG.vidInt vertex) False
  where
    handleEdge edge =
        unlessM (hasCycle state) $ do
            let w = DG.eToIdx edge
            wMarked <- Arr.readArray (marked state) (DG.vidInt w)
            if not wMarked
                then do
                    -- found new vertex, so recur
                    Arr.writeArray (edgeTo state) (DG.vidInt w) (Just edge)
                    dfs graph state w
                else
                    -- trace back directed cycle
                    whenM (Arr.readArray (onStack state) (DG.vidInt w)) $
                        traceBackCycle state (DG.vidInt w) edge
                            >>= MV.writeMutVar (cycle state)

-- | Trace back a cycle by starting from the last edge in the cycle,
--    and going back until we reach an edge with a "from"-vertex that
--    equals the given vertex.
traceBackCycle
    :: forall s g v meta.
       (Hashable v, Show v)
    => State s g v meta          -- ^ State
    -> Int          -- ^ Int where cycle ends (and starts)
    -> DG.IdxEdge v meta                    -- ^ The last edge of the cycle
    -> ST s [DG.IdxEdge v meta]
traceBackCycle state startVertex lastEdge =
    go (lastEdge NE.:| [])
  where
    go :: NE.NonEmpty (DG.IdxEdge v meta) -> ST s [DG.IdxEdge v meta]
    go accum@(edge NE.:| _) = do
        let fromVertex = DG.vidInt $ DG.eFromIdx edge
        if fromVertex /= startVertex
            then do
                newEdgeM <- Arr.readArray (edgeTo state) fromVertex
                let newEdge = fromMaybe (error missingEdgeError) newEdgeM
                    missingEdgeError = "BUG: edgeTo[edge.from()] not present for edge"
                go $ newEdge `NE.cons` accum
            else return $ NE.toList accum

hasCycle
    :: State s g v meta
    -> ST s Bool
hasCycle = fmap (not . null) . MV.readMutVar . cycle

-- | Create initial 'State'
initState
    :: DG.Digraph s v meta   -- ^ Graph
    -> ST s (State s g v meta)   -- ^ Initialized state
initState graph = do
    vertexCount <- fromIntegral <$> DG.vertexCount graph
    state <- State
        <$> Arr.newArray (0, vertexCount) False      -- marked
        <*> Arr.newArray (0, vertexCount) Nothing    -- edgeTo
        <*> Arr.newArray (0, vertexCount) False      -- onStack
        <*> MV.newMutVar []                          -- cycle
    return state

-- certify that digraph is either acyclic or has a directed cycle
check :: (Eq v, Hashable v, Show v, Show meta) => State s g v meta -> ST s Bool
check state = do
    whenM (hasCycle state) $
        MV.readMutVar (cycle state) >>= maybe (return ()) error . verifyCycle
    return True

-- | Verify that a given list of edges form a cycle.
--   'Nothing' on no errors, otherwise a 'Just' with the description
--      of the error.
verifyCycle :: (Eq v, Hashable v, E.DirectedEdge edge v meta, Show edge) => [edge] -> Maybe String
verifyCycle           [] =
    Just "empty list of cycle edges"
verifyCycle [singleEdge] =
    if E.fromNode singleEdge /= E.toNode singleEdge
        then Just $ "bad single-edge cycle: " ++ show singleEdge
        else Nothing
verifyCycle edges =
    let compareEdges (prev, Nothing) next =
            if E.toNode prev /= E.fromNode next
                then Just $ printf "cycle edges %s and %s not incident" (show prev) (show next)
                else Nothing
        compareEdges (_, err) _ = err
    in snd $
            foldl'  (\accum next -> (next, compareEdges accum next))
                    (last edges, Nothing)
                    edges
                    --  Starting with the last edge checks that "toNode last == fromNode first"
