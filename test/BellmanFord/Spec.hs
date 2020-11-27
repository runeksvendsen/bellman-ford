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
import qualified EmptyGraph
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
    [ -- Tasty.testGroup "passes 'check'"
        -- [ QS.testProperty "additive (all weights)"
        --     (\edges -> bellmanFord (+) (edges :: [TestEdge]))
        -- , QS.testProperty "multiplicative (positive weights)"
        --     (\edges -> bellmanFord (*) (map positiveWeight edges :: [TestEdge]))
        -- , QS.testProperty "additive (all weights) -log weight"
        --     (\edges -> bellmanFord (+) (map NegLog edges :: [NegLog TestEdge]))
        -- ]
     Tasty.testGroup "finds negative cycle" $
        [ -- Tasty.testGroup "WITHOUT zero-weight cycle"
            -- [ QS.testProperty "with no other edges in the graph" (findsNegativeCycle Nothing [])
            -- , QS.testProperty "with other (positive-weight) edges in the graph" (findsNegativeCycle Nothing)
            -- ]
         Tasty.testGroup "WITH zero-weight cycle"
            [ QS.testProperty "with no other edges in the graph" (findsNegativeCycleZero [])
            , QS.testProperty "with other (positive-weight) edges in the graph" findsNegativeCycleZero
            ]
        ]
    -- , Tasty.testGroup "removePaths"
    --    [ QS.testProperty "terminates" removePathsTerminates
    --    ]
    ]

removePathsTerminates :: [TestEdge] -> Expectation
removePathsTerminates edges = do
    ST.stToIO $ EmptyGraph.removePaths edges (getFrom $ edges !! 0)
    (0 :: Int) `shouldSatisfy` const True

bellmanFord
    :: (Lib.HasWeight a Double, Show a, Show v, Lib.DirectedEdge edge v a, Ord v, Eq a)
    => (Double -> Double -> Double)
    -> [edge]
    -> IO ()
bellmanFord combine edges = do
    graph <- fromShuffledEdges edges
    vertices <- ST.stToIO $ Lib.vertexLabels graph
    ST.stToIO $ forM_ vertices $ \source ->
        Lib.runBF graph (\weight edge -> weight `combine` Lib.weight edge) $
            Lib.bellmanFord source

findsNegativeCycleZero :: [PositiveWeight] -> ZeroCycle -> NegativeCycle -> Expectation
findsNegativeCycleZero positiveEdges zeroCycle negCycle =
    findsNegativeCycle (Just zeroCycle) positiveEdges negCycle

-- | When edges comprising a negative cycle are added to the graph,
--    along with an arbitrary number of positive-weight edges,
--    "Lib.negativeCycle" finds only one negative cycle, equal
--    to the list of input negative-cycle edges.
findsNegativeCycle
    :: Maybe ZeroCycle
    -> [PositiveWeight]
    -> NegativeCycle
    -> Expectation
findsNegativeCycle zeroCycleM positiveEdges (NegativeCycle cycleEdges) = do
    shuffledPositiveEdges <- Shuffle.shuffleM (map positiveWeight positiveEdges)
    shuffledCycleEdges <- Shuffle.shuffleM (NE.toList cycleEdges)
    shuffledZeroEdges <- Shuffle.shuffleM zeroCycleEdges
    graph <- ST.stToIO $ Lib.fromEdges (shuffledPositiveEdges ++ shuffledZeroEdges ++ shuffledCycleEdges)
    let cycleVertices = concat $ NE.map (\e -> [getFrom e, getTo e]) cycleEdges
    shuffledVertices <- Shuffle.shuffleM cycleVertices
    negativeCycleM <- ST.stToIO $ Lib.runBF graph weightCombFun $ do
        Lib.bellmanFord (head shuffledVertices)
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
            map Util.fromIdxEdge (NE.toList returnedCycle) `shouldSatisfy` (`Util.sameUniqueSequenceAs` NE.toList cycleEdges)
  where
    zeroCycleEdges = maybe [] (NE.toList . getZeroCycle) zeroCycleM
    weightCombFun weight edge = weight + Lib.weight edge

fromShuffledEdges
    :: (Ord v, Lib.DirectedEdge edge v meta)
    => [edge]
    -> IO (Lib.Digraph RealWorld v meta)
fromShuffledEdges edges =
    Shuffle.shuffleM edges >>= ST.stToIO . Lib.fromEdges
