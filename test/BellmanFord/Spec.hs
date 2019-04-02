{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module BellmanFord.Spec
( spec
)
where

import           Data.Graph.Prelude
import           Types.Edge
import           Types.Cycle
import qualified Data.Graph.Digraph                 as Lib
import qualified Data.Graph.BellmanFord             as Lib

import qualified Control.Monad.ST                   as ST
import qualified Test.Hspec.SmallCheck              ()
import           Test.Hspec.Expectations            (Expectation, shouldBe)
import qualified Test.Tasty                         as Tasty
import qualified Test.Tasty.SmallCheck              as SC
import qualified Data.List.NonEmpty                 as NE
import qualified System.Random.Shuffle              as Shuffle


spec :: Tasty.TestTree
spec = Tasty.testGroup "BellmanFord" $
    [ Tasty.testGroup "passes 'check'" $
        [ SC.testProperty "additive (all weights)" $ \edges ->
            bellmanFord (+) (edges :: [TestEdge])
        , SC.testProperty "multiplicative (positive weights)" $ \edges ->
            bellmanFord (*) (map positiveWeight edges :: [TestEdge])
        , SC.testProperty "additive (all weights) -log weight" $ \edges ->
            bellmanFord (+) (map NegLog edges :: [NegLog TestEdge])
        ]
    , SC.testProperty "finds negative cycle" findsNegativeCycle
    ]

bellmanFord
    :: (Lib.WeightedEdge e v Double, Eq e, Show e)
    => (Double -> Double -> Double)
    -> [e]
    -> Expectation
bellmanFord combine edges = do
    graph <- fromShuffledEdges edges
    vertices <- Lib.vertexLabels graph
    ST.stToIO $ forM_ vertices $ \source -> do
        Lib.bellmanFord graph source (\weight edge -> weight `combine` Lib.weight edge)

-- | When edges comprising a negative cycle are added to the graph,
--    along with an arbitrary number of positive-weight edges,
--    "Lib.negativeCycle" finds only one negative cycle, equal
--    to the list of input negative-cycle edges.
findsNegativeCycle
    :: NegativeCycle
    -> [PositiveWeight TestEdge]
    -> Expectation
findsNegativeCycle (NegativeCycle cycleEdges) positiveEdges = do
    graph <- fromShuffledEdges (map positiveWeight positiveEdges)
    mapM_ (Lib.insertEdge graph) =<< Shuffle.shuffleM (NE.toList cycleEdges)
    shuffledVertices <- Shuffle.shuffleM =<< Lib.vertexLabels graph
    negativeCycleM <- ST.stToIO $ do
        state <- Lib.bellmanFord graph (head shuffledVertices) weightCombFun
        Lib.negativeCycle state
    negativeCycleM `shouldBe` Just cycleEdges
  where
    weightCombFun weight edge = weight + Lib.weight edge

fromShuffledEdges edges =
    Shuffle.shuffleM edges >>= Lib.fromEdges
