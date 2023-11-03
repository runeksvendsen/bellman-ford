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
import qualified Data.Graph.BellmanFord.Unboxed     as Lib

import qualified Control.Monad.ST                   as ST
import qualified Test.Hspec.SmallCheck              ()
import           Test.Hspec.Expectations.Pretty            ( Expectation
                                                    , shouldSatisfy
                                                    , expectationFailure
                                                    )
import qualified Test.Tasty                         as Tasty
import qualified Data.List.NonEmpty                 as NE
import qualified System.Random.Shuffle              as Shuffle
import qualified Data.Graph.SP.Double as Double
import qualified Test.QuickCheck as QC
import qualified Test.SmallCheck.Series as SS
import Data.Int (Int64, Int32)
import IndexMinPQ.Util (Positive, unPositive)

spec :: Tasty.TestTree
spec = Tasty.testGroup "BellmanFord"
    [ Tasty.testGroup "edge weight: Double"
        [ testPassesCheck id Double.isLessThan (1/0)
        , testFindsNegativeCycle id Double.isLessThan (1/0)
        , testRemovePaths id Double.isLessThan (1/0)
        ]
    , Tasty.testGroup "edge weight: Int64"
        [ testPassesCheck boundedInt64 int64IsLessThan maxBound
        , testFindsNegativeCycle boundedInt64 int64IsLessThan maxBound
        , testRemovePaths boundedInt64 int64IsLessThan maxBound
        ]
    ]
    where
        int64IsLessThan :: Int64 -> Int64 -> Bool
        int64IsLessThan = (<)

        boundedInt64 :: BoundedIntegral Int32 Int64 -> Int64
        boundedInt64 = getBoundedIntegral

        testPassesCheck
            :: forall weight generatedWeight.
            ( Show weight, Eq weight, Num weight
            , Show generatedWeight
            , QC.Arbitrary weight
            , SS.Serial IO weight
            , QC.Arbitrary generatedWeight
            , SS.Serial IO generatedWeight
            , Lib.Unboxable weight RealWorld
            )
            => (generatedWeight -> weight) -- Allows using e.g. 'Test.QuickCheck.Positive' as weight by passing in 'getPositive' here
            -> (weight -> weight -> Bool)
            -> weight
            -> Tasty.TestTree
        testPassesCheck unGeneratedWeight isLessThan infinity = Tasty.testGroup "passes 'check'"
            [ QS.testProperty "additive (all weights)"
                (\edges ->
                    bellmanFord
                        (+)
                        isLessThan
                        0
                        infinity
                        (map (fmap unGeneratedWeight) edges :: [TestEdge weight])
                )
            , QS.testProperty "multiplicative (positive weights)"
                True -- TODO: re-enable once https://github.com/runeksvendsen/bellman-ford/issues/5 is fixed
            , QS.testProperty "additive (all weights) -log weight"
                -- TODO: NegLog?
                (\edges ->
                    bellmanFord
                        (+)
                        isLessThan
                        0
                        infinity
                        (map (fmap unGeneratedWeight) edges :: [TestEdge weight])
                )
            ]

        testRemovePaths unGeneratedWeight isLessThan infinity = Tasty.testGroup "removePaths"
            [ QS.testProperty "terminates" $ removePathsTerminates unGeneratedWeight isLessThan infinity
            ]

        testFindsNegativeCycle unGeneratedWeight isLessThan infinity = Tasty.testGroup "finds negative cycle"
            [ QS.testProperty "with no other edges in the graph" True -- TODO: re-enable once https://github.com/runeksvendsen/bellman-ford/issues/6 is fixed
            , QS.testProperty "with other (positive-weight) edges in the graph" $
                findsNegativeCycle unGeneratedWeight isLessThan 0 infinity
            ]

removePathsTerminates
    :: ( Lib.Unboxable weight RealWorld
       , Show weight, Eq weight, Num weight
       )
    => (generatedWeight -> weight)
    -> (weight -> weight -> Bool)
    -> weight
    -> [TestEdge generatedWeight]
    -> Expectation
removePathsTerminates unGeneratedWeight isLessThan infinity edges = do
    ST.stToIO $ EmptyGraph.removePaths isLessThan 0 infinity (map (fmap unGeneratedWeight) edges) (getFrom $ edges !! 0)
    (0 :: Int) `shouldSatisfy` const True

bellmanFord
    :: ( Show v, Ord v
       , Eq weight, Show weight
       , Eq edge, Show edge
       , Lib.DirectedEdge edge v weight
       , Lib.Unboxable weight RealWorld
       )
    => (weight -> weight -> weight)
    -> (weight -> weight -> Bool)
    -> weight
    -> weight
    -> [edge]
    -> IO ()
bellmanFord combine isLessThan zero infinity edges = do
    graph <- fromShuffledEdges edges
    vertices <- ST.stToIO $ Lib.vertexLabels graph
    ST.stToIO $ forM_ vertices $ \source ->
        Lib.runBF graph combine isLessThan zero infinity $
            Lib.bellmanFord source

-- | When edges comprising a negative cycle are added to the graph,
--    along with an arbitrary number of positive-weight edges,
--    "Lib.negativeCycle" finds only one negative cycle, equal
--    to the list of input negative-cycle edges.
findsNegativeCycle
    :: (Lib.Unboxable weight RealWorld, Eq weight, Show weight, Num weight)
    => (generatedWeight -> weight)
    -> (weight -> weight -> Bool)
    -> weight
    -> weight
    -> [TestEdge (Positive generatedWeight)]
    -> NegativeCycle generatedWeight
    -> Expectation
findsNegativeCycle unGeneratedWeight isLessThan zero infinity positiveEdges (NegativeCycle cycleEdges) = do
    shuffledPositiveEdges <- Shuffle.shuffleM positiveEdges'
    shuffledCycleEdges <- Shuffle.shuffleM (NE.toList cycleEdges')
    graph <- ST.stToIO $ Lib.fromEdges (shuffledPositiveEdges ++ shuffledCycleEdges)
    let cycleVertices = concat $ NE.map (\e -> [getFrom e, getTo e]) cycleEdges
    shuffledVertices <- Shuffle.shuffleM cycleVertices
    negativeCycleM <- ST.stToIO $ Lib.runBF graph (+) isLessThan zero infinity $ do
        Lib.bellmanFord (head shuffledVertices)
        Lib.negativeCycle
    case negativeCycleM of
        Nothing ->
            let errFormatStr = unlines
                    [ "no cycle found."
                    , "expected: %s"
                    , "positive edges: %s"
                    ]
            in expectationFailure $ printf errFormatStr (show cycleEdges') (show positiveEdges')
        Just returnedCycle ->
            map Util.fromIdxEdge (NE.toList returnedCycle) `shouldSatisfy` (`Util.sameUniqueSequenceAs` NE.toList cycleEdges')
    where
        positiveEdges' = map (fmap (unGeneratedWeight . unPositive)) positiveEdges
        cycleEdges' = NE.map (fmap unGeneratedWeight) cycleEdges

fromShuffledEdges
    :: (Ord v, Lib.DirectedEdge edge v weight)
    => [edge]
    -> IO (Lib.Digraph RealWorld v weight)
fromShuffledEdges edges =
    Shuffle.shuffleM edges >>= ST.stToIO . Lib.fromEdges
