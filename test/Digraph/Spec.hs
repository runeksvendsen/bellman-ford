{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Digraph.Spec
( spec
)
where

import           Types.Edge
import           Data.Graph.Prelude
import qualified Data.Graph.Digraph                 as Lib
import           Data.List                          (sort, nubBy)

import qualified Test.Hspec.SmallCheck              ()
import           Test.Hspec.Expectations            (Expectation, shouldBe)
import qualified Test.Tasty                         as Tasty
import           Test.Tasty.SmallCheck              as SC


spec :: Tasty.TestTree
spec = Tasty.testGroup "Digraph" $
    [ Tasty.testGroup "removeEdge"
        [ SC.testProperty "removes all vertices' outgoing edges" $ \lol -> do
            -- print lol
            addRemoveEdges lol
        ]
    , Tasty.testGroup "insertEdge"
        [ SC.testProperty "all edges present in 'outoingEdges'" addEdgesCheckOutgoing
        ]
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
