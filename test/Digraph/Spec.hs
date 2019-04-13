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

import qualified Test.Hspec.SmallCheck              ()
import           Test.Hspec.Expectations            (Expectation, shouldBe)
import qualified Test.Tasty                         as Tasty


spec :: Tasty.TestTree
spec = Tasty.testGroup "Digraph" $
    [ Tasty.testGroup "removeEdge" $
        QS.testProperty "removes all vertices' outgoing edges" addRemoveEdges
    , Tasty.testGroup "insertEdge" $
        QS.testProperty "all edges present in 'outoingEdges'" addEdgesCheckOutgoing
    , Tasty.testGroup "edgeCount" $
        QS.testProperty "== outgoing edge count for all vertices" edgeCountEqualsOutgoingCountForallVertices
    ]

addRemoveEdges
    :: [TestEdge]
    -> Expectation
addRemoveEdges edges = do
    graph <- Lib.fromEdges edges
    forM_ edges (Lib.removeEdge graph)
    vertices <- Lib.vertices graph
    forM_ vertices $ \vertex -> do
        outEdges <- Lib.outgoingEdges graph vertex
        length outEdges `shouldBe` 0

addEdgesCheckOutgoing
    :: [TestEdge]
    -> Expectation
addEdgesCheckOutgoing edges = do
    let sortedEdges = sort edges
    -- inserting edges in reverse order makes sure the *first* edge in "sortedEdges"
    --  (going from/to same vertex) will be present in graph in the end
    graph <- Lib.fromEdges (reverse sortedEdges)
    vertices <- Lib.vertices graph
    outgoingEdges <- foldM (collectOutgoing graph) [] vertices
    sort (concat outgoingEdges) `shouldBe` removeDuplicateSrcDst sortedEdges
  where
    collectOutgoing graph accum vertex = do
        outEdges <- Lib.outgoingEdges graph vertex
        return $ outEdges : accum
    removeDuplicateSrcDst = nubBy sameSrcDst
    sameSrcDst edgeA edgeB =
        getFrom edgeA == getFrom edgeB &&
        getTo edgeA == getTo edgeB

edgeCountEqualsOutgoingCountForallVertices
    :: [TestEdge]
    -> Expectation
edgeCountEqualsOutgoingCountForallVertices edges = do
    graph <- Lib.fromEdges edges
    edgeCountLib      <- Lib.edgeCount graph
    edgeCountOutgoing <- edgeCountTest graph
    edgeCountLib `shouldBe` edgeCountOutgoing

-- | Count of the number of edges in the graph
--    by counting all outgoing edges for all vertices returned by 'Lib.vertices'.
--   Should always return the same as 'Lib.edgeCount'.
edgeCountTest
    :: (PrimMonad m)
    => Lib.Digraph (PrimState m) g e v  -- ^ Graph
    -> m Word                           -- ^ Edge count
edgeCountTest dg =
    Lib.vertices dg >>= foldM lookupCount 0
  where
    lookupCount totalCount vertex =
        Lib.outgoingEdges dg vertex >>=
            foldM (\innerCount _ -> return $ innerCount+1) totalCount
