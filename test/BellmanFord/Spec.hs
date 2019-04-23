{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module BellmanFord.Spec
( spec
)
where

import qualified Util.QuickSmall                    as QS
import qualified Util
import           Data.Graph.Prelude
import           Types.Edge
import           Types.Cycle
import qualified Data.Graph.Digraph                 as Lib
import qualified Data.Graph.BellmanFord             as Lib

import qualified Control.Monad.ST                   as ST
import qualified Test.Hspec.SmallCheck              ()
import           Test.Hspec.Expectations            ( Expectation
                                                    , shouldSatisfy
                                                    , expectationFailure
                                                    )
import qualified Test.Tasty                         as Tasty
import qualified Data.List.NonEmpty                 as NE
import qualified System.Random.Shuffle              as Shuffle
import           Text.Printf                        (printf)


spec :: Tasty.TestTree
spec = Tasty.testGroup "BellmanFord"
    [ Tasty.testGroup "passes 'check'"
        [ QS.testProperty "additive (all weights)"
            (\edges -> bellmanFord (+) (edges :: [TestEdge]))
        , QS.testProperty "multiplicative (positive weights)"
            (\edges -> bellmanFord (*) (map positiveWeight edges :: [TestEdge]))
        , QS.testProperty "additive (all weights) -log weight"
            (\edges -> bellmanFord (+) (map NegLog edges :: [NegLog TestEdge]))
        ]
    , Tasty.testGroup "finds negative cycle"
       [ QS.testProperty "with no other edges in the graph" (findsNegativeCycle [])
       , QS.testProperty "with other (positive-weight) edges in the graph" findsNegativeCycle
       ]
    ]

bellmanFord
    :: (Lib.WeightedEdge e v Double, Eq e, Show e)
    => (Double -> Double -> Double)
    -> [e]
    -> Expectation
bellmanFord combine edges = do
    graph <- fromShuffledEdges edges
    vertices <- Lib.vertexLabels graph
    ST.stToIO $ forM_ vertices $ \source ->
        Lib.runBF graph (\weight edge -> weight `combine` Lib.weight edge) source $
            Lib.bellmanFord

-- | When edges comprising a negative cycle are added to the graph,
--    along with an arbitrary number of positive-weight edges,
--    "Lib.negativeCycle" finds only one negative cycle, equal
--    to the list of input negative-cycle edges.
findsNegativeCycle
    :: [PositiveWeight]
    -> NegativeCycle
    -> Expectation
findsNegativeCycle positiveEdges (NegativeCycle cycleEdges) = do
    graph <- fromShuffledEdges (map positiveWeight positiveEdges)
    mapM_ (Lib.insertEdge graph) =<< Shuffle.shuffleM (NE.toList cycleEdges)
    let cycleVertices = concat $ NE.map (\e -> [getFrom e, getTo e]) cycleEdges
    shuffledVertices <- Shuffle.shuffleM cycleVertices
    let srcVertex = head shuffledVertices
    negativeCycleM <- ST.stToIO $ Lib.runBF graph weightCombFun srcVertex $ do
        Lib.bellmanFord
        Lib.negativeCycle
    case negativeCycleM of
        Nothing ->
            let errFormatStr = unlines
                    [ "no cycle found."
                    , "expected: %s"
                    , "positive edges: %s"
                    ]
            in expectationFailure $ printf errFormatStr (show cycleEdges) (show positiveEdges)
        Just returnedCycle ->
            NE.toList returnedCycle `shouldSatisfy` (`Util.sameUniqueSequenceAs` NE.toList cycleEdges)
  where
    weightCombFun weight edge = weight + Lib.weight edge

fromShuffledEdges edges =
    Shuffle.shuffleM edges >>= Lib.fromEdges
