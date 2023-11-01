{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Digraph.Spec
( spec
)
where

import           Types.Edge
import qualified Util
import qualified Util.QuickSmall                    as QS

import           Data.Graph.Prelude
import qualified Data.Graph.Digraph                 as Lib
import           Data.List                          (groupBy, sortOn, sort)

import qualified Test.Hspec.SmallCheck              ()
import           Test.Hspec.Expectations.Pretty            (Expectation, shouldBe)
import qualified Test.Tasty                         as Tasty


spec :: Tasty.TestTree
spec = Tasty.testGroup "Digraph" $
    [ Tasty.testGroup "removeEdge"
       [ QS.testProperty "removes all vertices' outgoing edges" (addRemoveEdges @Double)
       ]
    , Tasty.testGroup "updateEdge"
       [ QS.testProperty "updates edges present in 'outgoingEdges'" (updateEdges @Double)
       ]
    , Tasty.testGroup "insertEdge"
       [ QS.testProperty "all edges present in 'outgoingEdges'" (addEdgesCheckInOutgoing @Double)
       ]
    , Tasty.testGroup "edgeCount"
       [ QS.testProperty "== outgoing edge count for all vertices" (edgeCountEqualsOutgoingCountForallVertices @Double)
       ]
    ]

addRemoveEdges
    :: [TestEdge weight]
    -> Expectation
addRemoveEdges edges = do
    (graph, vertices) <- stToIO createRemove
    forM_ vertices $ \vertex -> do
        outEdges <- stToIO $ Lib.outgoingEdges graph vertex
        length outEdges `shouldBe` 0
  where
    createRemove = do
        graph <- Lib.fromEdges edges
        outgoingEdges <- collectOutgoing graph
        forM_ outgoingEdges (Lib.removeEdge graph)
        vertices <- Lib.vertices graph
        return (graph, vertices)

updateEdges
    :: (Ord weight, Show weight, Num weight)
    => [TestEdge weight]
    -> IO ()
updateEdges edges = do
    graph <- stToIO $ Lib.fromEdges edges
    outgoingEdges <- stToIO $ collectOutgoing graph
    let outgoingEdges' = fmap (+1) <$> outgoingEdges
    forM_ outgoingEdges' (stToIO . Lib.updateEdge graph)
    updatedOutgoingEdges <- stToIO $ collectOutgoing graph
    sort (fmap Lib.eMeta updatedOutgoingEdges) `shouldBe`
        sort (map getWeight (removeDuplicateSrcDst $ map Util.fromIdxEdge outgoingEdges'))

addEdgesCheckInOutgoing
    :: (Ord weight, Show weight)
    => [TestEdge weight]
    -> Expectation
addEdgesCheckInOutgoing edges = do
    let sortedEdges = sort edges
    -- inserting edges in reverse order makes sure the *first* edge in "edges"
    --  (in case of duplicate src/dst vertex) will be present in graph in the end
    graph <- stToIO $ Lib.fromEdges (reverse sortedEdges)
    outgoingEdges <- stToIO $ collectOutgoing graph
    sort (map Util.fromIdxEdge outgoingEdges) `shouldBe` removeDuplicateSrcDst sortedEdges

removeDuplicateSrcDst :: [TestEdge weight] -> [TestEdge weight]
removeDuplicateSrcDst =
    map head . groupBy sameSrcDst . sortOn (\e -> (Lib.fromNode e, Lib.toNode e))
  where
    srcDst e = (Lib.fromNode e, Lib.toNode e)
    sameSrcDst edgeA edgeB = srcDst edgeA == srcDst edgeB

collectOutgoing :: Lib.Digraph s v meta -> ST s [Lib.IdxEdge v meta]
collectOutgoing graph = do
    vertices <- Lib.vertices graph
    concat <$> foldM collect [] vertices
  where
    collect accum vertex = do
        outEdges <- Lib.outgoingEdges graph vertex
        return $ outEdges : accum

edgeCountEqualsOutgoingCountForallVertices
    :: [TestEdge weight]
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
