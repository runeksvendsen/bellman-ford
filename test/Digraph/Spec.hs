{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Digraph.Spec
( spec
)
where

import           Digraph.Types                      (ModifyGraph(..), modify)
import           Types.Edge
import qualified Util.QuickSmall                    as QS

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
    [ Tasty.testGroup "removeEdge" $
        QS.testProperty "removes all vertices' outgoing edges" addRemoveEdges
    , Tasty.testGroup "insertEdge" $
        QS.testProperty "all edges present in 'outoingEdges'" (addEdgesCheckInOutgoing Lib.outgoingEdges)
        ++ QS.testProperty "all edges present in 'incomingEdges'" (addEdgesCheckInOutgoing Lib.incomingEdges)
    , Tasty.testGroup "edgeCount" $
        QS.testProperty "== outgoing edge count for all vertices" edgeCountEqualsOutgoingCountForallVertices
    , Tasty.localOption (SC.SmallCheckDepth 4) $
      Tasty.testGroup "outgoing/incoming edges" $
        QS.testProperty "the same after random inserts/removals" incomingShouldBeOutgoing
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
    sameSrcDst edgeA edgeB =
        getFrom edgeA == getFrom edgeB &&
        getTo edgeA == getTo edgeB

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
