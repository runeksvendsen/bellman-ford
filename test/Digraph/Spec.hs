{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Digraph.Spec
( spec
)
where

import           Types.Edge
import qualified Util.QuickSmall                    as QS

import           Data.Graph.Prelude
import qualified Data.Graph.Digraph                 as Lib
import           Data.List                          (sort, nubBy)
import           Control.Monad.ST                   (RealWorld, stToIO)

import qualified Test.Hspec.SmallCheck              ()
import           Test.Hspec.Expectations            (Expectation, shouldBe)
import qualified Test.Tasty                         as Tasty
import           Test.Tasty.SmallCheck              as SC


spec :: Tasty.TestTree
spec = Tasty.testGroup "Digraph" $
    [ Tasty.testGroup "removeEdge"
       [ QS.testProperty "removes all vertices' outgoing edges" addRemoveEdges
       ]
    , Tasty.testGroup "insertEdge"
       [ QS.testProperty "all edges present in 'outoingEdges'" (addEdgesCheckInOutgoing $ \g vid -> stToIO $ Lib.outgoingEdges g vid)
       ]
    , Tasty.testGroup "edgeCount"
       [ QS.testProperty "== outgoing edge count for all vertices" edgeCountEqualsOutgoingCountForallVertices
       ]
    ]

addRemoveEdges
    :: [TestEdge]
    -> Expectation
addRemoveEdges edges = do
    (graph, vertices) <- hey
    forM_ vertices $ \vertex -> do
        outEdges <- stToIO $ Lib.outgoingEdges graph vertex
        length outEdges `shouldBe` 0
  where
    hey = do
        graph <- stToIO $ Lib.fromEdges edges
        forM_ edges (stToIO . Lib.removeEdge graph)
        vertices <- stToIO $ Lib.vertices graph
        return (graph, vertices)

addEdgesCheckInOutgoing
    :: (Lib.Digraph RealWorld String Double -> Lib.VertexId -> IO [edge])
    -> [TestEdge]
    -> Expectation
addEdgesCheckInOutgoing inOutEdges edges = do
    let sortedEdges = sort edges
    -- inserting edges in reverse order makes sure the *first* edge in "sortedEdges"
    --  (going from/to same vertex) will be present in graph in the end
    graph <- stToIO $ Lib.fromEdges (reverse sortedEdges)
    vertices <- stToIO $ Lib.vertices graph
    outgoingEdges <- foldM (collectInOutgoing graph) [] vertices
    sort (concat outgoingEdges) `shouldBe` removeDuplicateSrcDst sortedEdges
  where
    collectInOutgoing graph accum vertex = do
        outEdges <- inOutEdges graph vertex
        return $ outEdges : accum
    removeDuplicateSrcDst = nubBy sameSrcDst
    sameSrcDst edgeA edgeB =
        Lib.fromNode edgeA == Lib.fromNode edgeB &&
        Lib.toNode edgeA == Lib.toNode edgeB

edgeCountEqualsOutgoingCountForallVertices
    :: [TestEdge]
    -> Expectation
edgeCountEqualsOutgoingCountForallVertices edges = do
    graph <- stToIO $ Lib.fromEdges edges
    edgeCountLib      <- stToIO $ Lib.edgeCount graph
    edgeCountOutgoing <- stToIO $ edgeCountTest graph
    edgeCountLib `shouldBe` edgeCountOutgoing

-- | Count of the number of edges in the graph
--    by counting all outgoing edges for all vertices returned by 'Lib.vertices'.
--   Should always return the same as 'Lib.edgeCount'.
edgeCountTest
    :: Lib.Digraph s v meta -- ^ Graph
    -> ST s Word            -- ^ Edge count
edgeCountTest dg =
    Lib.vertices dg >>= foldM lookupCount 0
  where
    lookupCount totalCount vertex =
        Lib.outgoingEdges dg vertex >>=
            foldM (\innerCount _ -> return $ innerCount+1) totalCount
