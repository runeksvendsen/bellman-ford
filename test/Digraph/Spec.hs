{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Digraph.Spec
( spec
)
where

import           Digraph.Types                      (ModifyGraph(..), modify)
import           Edge.Types
import           Data.Graph.Prelude
import qualified Data.Graph.Digraph                 as Lib
import           Data.List                          (sort, nubBy)
import           Control.Monad.ST                   (RealWorld)

import qualified Test.Hspec.SmallCheck              ()
import           Test.Hspec.Expectations            (Expectation, shouldBe)
import qualified Test.Tasty                         as Tasty
import           Test.Tasty.SmallCheck              as SC


spec :: Tasty.TestTree
spec = Tasty.testGroup "Digraph" $
    [ Tasty.testGroup "removeEdge"
        [ SC.testProperty "removes all vertices' outgoing edges" addRemoveEdges
        ]
    , Tasty.testGroup "insertEdge"
        [ SC.testProperty "all edges present in 'outoingEdges'" (addEdgesCheckInOutgoing Lib.outgoingEdges)
        , SC.testProperty "all edges present in 'incomingEdges'" (addEdgesCheckInOutgoing Lib.incomingEdges)
        ]
    , Tasty.localOption (SC.SmallCheckDepth 5) $
      Tasty.testGroup "outgoing/incoming edges"
        [ SC.testProperty "the same after random inserts/removals" incomingShouldBeOutgoing
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

addEdgesCheckInOutgoing
    :: (Lib.Digraph RealWorld () TestEdge String -> Lib.Vertex () -> IO [TestEdge])
    -> [TestEdge]
    -> Expectation
addEdgesCheckInOutgoing inOutEdges edges = do
    let sortedEdges = sort edges
    -- inserting edges in reverse order makes sure the *first* edge in "sortedEdges"
    --  (going from/to same vertex) will be present in graph in the end
    graph <- Lib.fromEdges (reverse sortedEdges)
    vertices <- Lib.vertices graph
    outgoingEdges <- foldM (collectInOutgoing graph) [] vertices
    sort (concat outgoingEdges) `shouldBe` removeDuplicateSrcDst sortedEdges
  where
    collectInOutgoing graph accum vertex = do
        outEdges <- inOutEdges graph vertex
        return $ outEdges : accum
    removeDuplicateSrcDst = nubBy sameSrcDst
    sameSrcDst edgeA edgeB = getEdge edgeA == getEdge edgeB

incomingShouldBeOutgoing
    :: [ModifyGraph (Unweighted TestEdge)]
    -> Expectation
incomingShouldBeOutgoing modifyGraphsUnweighted = do
    let modifyGraphs = map (fmap unweighted) modifyGraphsUnweighted
    (outEdges, inEdges) <- Lib.withGraph $ \graph -> do
        forM_ modifyGraphs (modify graph)
        vertices <- Lib.vertices graph
        outEdges <- foldM (collectInOutgoing Lib.outgoingEdges graph) [] vertices
        inEdges <- foldM (collectInOutgoing Lib.incomingEdges graph) [] vertices
        return (concat outEdges, concat inEdges)
    sort outEdges `shouldBe` sort inEdges
  where
    collectInOutgoing inOutEdges graph accum vertex =
        (: accum) <$> inOutEdges graph vertex
