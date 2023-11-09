{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module BellmanFord.Spec
( spec
, RunBF
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
import qualified Data.Graph.SP.Int64

type RunBF weight s v meta a =
       (Lib.Unboxable weight s, Show meta, Show v, Show weight)
    => Lib.Digraph s v meta
    -> (weight -> meta -> weight)
    -> (weight -> weight -> Bool)
    -> weight
    -> weight
    -> Lib.BF s v weight meta a
    -> ST s a

spec :: (forall weight s v meta a. RunBF weight s v meta a) -> Tasty.TestTree
spec runBF = Tasty.testGroup "BellmanFord"
    [ Tasty.testGroup "Double"
        [ testPassesCheck id (+) Double.isLessThan (1/0)
        , testFindsNegativeCycle id (+) Double.isLessThan (1/0)
        , testRemovePaths id (+) Double.isLessThan (1/0)
        ]
    , Tasty.testGroup "Int64"
        [ testPassesCheck boundedInt64 int64Addition int64IsLessThan int64Infinity
        , testFindsNegativeCycle boundedInt64 int64Addition int64IsLessThan int64Infinity
        , testRemovePaths boundedInt64 int64Addition int64IsLessThan int64Infinity
        ]
    ]
    where
        int64IsLessThan :: Int64 -> Int64 -> Bool
        int64IsLessThan = (<)

        int64Addition :: Int64 -> Int64 -> Int64
        int64Addition = Data.Graph.SP.Int64.addition

        int64Infinity :: Int64
        int64Infinity = Data.Graph.SP.Int64.infinity

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
            -> (weight -> weight -> weight)
            -> (weight -> weight -> Bool)
            -> weight
            -> Tasty.TestTree
        testPassesCheck unGeneratedWeight combine isLessThan infinity = Tasty.testGroup "passes 'check'"
            [ QS.testProperty "additive (all weights)"
                (\edges ->
                    bellmanFord
                        runBF
                        combine
                        isLessThan
                        0
                        infinity
                        (map (fmap unGeneratedWeight) edges :: [TestEdge weight])
                )
            , QS.testProperty "multiplicative (positive weights)"
                True -- TODO: re-enable once https://github.com/runeksvendsen/bellman-ford/issues/5 is fixed
              -- TODO: NegLog?
            ]

        testRemovePaths unGeneratedWeight combine isLessThan infinity = Tasty.testGroup "removePaths"
            [ QS.testProperty "terminates" $ removePathsTerminates runBF unGeneratedWeight combine isLessThan infinity
            ]

        testFindsNegativeCycle unGeneratedWeight combine isLessThan infinity = Tasty.testGroup "finds negative cycle"
            [ QS.testProperty "with no other edges in the graph" $
                findsNegativeCycle runBF unGeneratedWeight combine isLessThan 0 infinity []
            , QS.testProperty "with other (positive-weight) edges in the graph" $ \positiveEdges ->
                findsNegativeCycle runBF unGeneratedWeight combine isLessThan 0 infinity (NE.toList positiveEdges)
            ]

removePathsTerminates
    :: ( Lib.Unboxable weight RealWorld
       , Show weight, Eq weight, Num weight
       )
    => (forall weight' s v meta a. RunBF weight' s v meta a)
    -> (generatedWeight -> weight)
    -> (weight -> weight -> weight)
    -> (weight -> weight -> Bool)
    -> weight
    -> [TestEdge generatedWeight]
    -> Expectation
removePathsTerminates runBF unGeneratedWeight combine isLessThan infinity edges = do
    ST.stToIO $ do
        graph <- Lib.fromEdges edges'
        bfAction <- EmptyGraph.removePaths graph (getFrom $ head edges)
        runBF graph combine isLessThan 0 infinity bfAction
    where
        edges' = map (fmap unGeneratedWeight) edges

bellmanFord
    :: ( Show v, Ord v
       , Eq weight, Show weight
       , Eq edge, Show edge
       , Lib.DirectedEdge edge v weight
       , Lib.Unboxable weight RealWorld
       )
    => (forall weight' s v' meta a. RunBF weight' s v' meta a)
    -> (weight -> weight -> weight)
    -> (weight -> weight -> Bool)
    -> weight
    -> weight
    -> [edge]
    -> IO ()
bellmanFord runBF combine isLessThan zero infinity edges = do
    graph <- fromShuffledEdges edges
    vertices <- ST.stToIO $ Lib.vertexLabels graph
    ST.stToIO $ forM_ vertices $ \source ->
        runBF graph combine isLessThan zero infinity $
            Lib.bellmanFord source

-- | When edges comprising a negative cycle are added to the graph,
--    along with an arbitrary number of positive-weight edges,
--    "Lib.negativeCycle" finds only one negative cycle, equal
--    to the list of input negative-cycle edges.
findsNegativeCycle
    :: (Lib.Unboxable weight RealWorld, Eq weight, Show weight, Num weight)
    => (forall weight' s v meta a. RunBF weight' s v meta a)
    -> (generatedWeight -> weight)
    -> (weight -> weight -> weight)
    -> (weight -> weight -> Bool)
    -> weight
    -> weight
    -> [TestEdge (Positive generatedWeight)]
    -> NegativeCycle generatedWeight
    -> Expectation
findsNegativeCycle runBF unGeneratedWeight combine isLessThan zero infinity positiveEdges (NegativeCycle cycleEdges) = do
    shuffledPositiveEdges <- Shuffle.shuffleM positiveEdges'
    shuffledCycleEdges <- Shuffle.shuffleM (NE.toList cycleEdges')
    graph <- ST.stToIO $ Lib.fromEdges (shuffledPositiveEdges ++ shuffledCycleEdges)
    let cycleVertices = concat $ NE.map (\e -> [getFrom e, getTo e]) cycleEdges
    shuffledVertices <- Shuffle.shuffleM cycleVertices
    negativeCycleM <- ST.stToIO $ runBF graph combine isLessThan zero infinity $ do
        Lib.bellmanFord (head shuffledVertices)
        Lib.negativeCycle
    case negativeCycleM of
        Nothing ->
            let errorStr = unlines
                    [ "no negative cycle found."
                    , "expected cycle: " <> show cycleEdges'
                    , "cycle weight: " <> show (sum $ NE.map getWeight cycleEdges')
                    , "cycle length: " <> show (length cycleEdges')
                    , "positive edges: " <> show positiveEdges'
                    ]
            in expectationFailure errorStr
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
