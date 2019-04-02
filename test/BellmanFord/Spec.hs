{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module BellmanFord.Spec
( spec
)
where

import qualified Util.QuickSmall                    as QS
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
-- import qualified Test.Tasty.SmallCheck              as SC
import qualified Data.List.NonEmpty                 as NE
import qualified System.Random.Shuffle              as Shuffle
import           Text.Printf                        (printf)


spec :: Tasty.TestTree
spec = Tasty.testGroup "BellmanFord" $
    [ Tasty.testGroup "passes 'check'" $
           QS.testProperty "additive (all weights)"
                (\edges -> bellmanFord (+) (edges :: [TestEdge]))
        ++ QS.testProperty "multiplicative (positive weights)"
                (\edges -> bellmanFord (*) (map positiveWeight edges :: [TestEdge]))
        ++ QS.testProperty "additive (all weights) -log weight"
            (\edges -> bellmanFord (+) (map NegLog edges :: [NegLog TestEdge]))
    , Tasty.testGroup "finds negative cycle" $
          QS.testProperty "with no other edges in the graph" (findsNegativeCycle [])
       ++ QS.testProperty "with other (positive-weight) edges in the graph" findsNegativeCycle
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
    :: [PositiveWeight]
    -> NegativeCycle
    -> Expectation
findsNegativeCycle positiveEdges (NegativeCycle cycleEdges) = do
    graph <- fromShuffledEdges (map positiveWeight positiveEdges)
    -- graph <- Lib.fromEdges []
    mapM_ (Lib.insertEdge graph) =<< Shuffle.shuffleM (NE.toList cycleEdges)
    let cycleVertices = concat $ NE.map (\e -> [getFrom e, getTo e]) cycleEdges
    shuffledVertices <- Shuffle.shuffleM cycleVertices
    negativeCycleM <- ST.stToIO $ do
        state <- Lib.bellmanFord graph (head shuffledVertices) weightCombFun
        Lib.negativeCycle state
    case negativeCycleM of
        Nothing ->
            let errFormatStr = unlines
                    [ "no cycle found."
                    , "expected: %s"
                    , "positive edges: %s"
                    ]
            in expectationFailure $ printf errFormatStr (show cycleEdges) (show positiveEdges)
        Just returnedCycle ->
            returnedCycle `shouldSatisfy` \cycle' ->
                cycle' == cycleEdges || cycle' == NE.reverse cycleEdges
                -- The returned cycle may be in the opposite direction
                --  of the input cycle
  where
    weightCombFun weight edge = weight + Lib.weight edge

fromShuffledEdges edges =
    Shuffle.shuffleM edges >>= Lib.fromEdges
