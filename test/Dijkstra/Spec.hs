{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE MultiWayIf #-}
module Dijkstra.Spec
( spec
)
where

import           Data.Graph.Prelude
import           Types.Edge
import           Types.Graph
import qualified Data.Graph.Digraph                 as Lib
import qualified Data.Graph.BellmanFord.Double      as BellmanFord
import qualified Data.Graph.Dijkstra                as Dijkstra

import qualified Control.Monad.Reader               as R
import qualified Control.Monad.ST                   as ST
import qualified Test.Hspec.SmallCheck              ()
import           Test.Hspec.Expectations.Pretty
import qualified Test.Tasty                         as Tasty
import qualified Test.QuickCheck as QC
import qualified Util.QuickSmall as QS
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import qualified Test.Tasty.QuickCheck
import qualified Data.Graph.Util
import qualified Test.Tasty.QuickCheck as TQC

testGraph1
    :: ( [TestEdge Double] -- graph edges
       , [((String, String), [TestEdge Double])] -- list of ((src, dst), expectedPath)
       )
testGraph1 = (,expectedPaths)
    [ (1 --> 2) 1.0
    , (2 --> 3) 0.5
    , (1 --> 3) 2.0
    ]
    where
        (-->) :: Int -> Int -> Double -> TestEdge Double
        a --> b = TestEdge (show a) (show b)

        -- ((src, dst), path)
        expectedPaths :: [((String, String), [TestEdge Double])]
        expectedPaths =
            [(("1", "3"), [(1 --> 2) 1.0, (2 --> 3) 0.5])]

spec :: Tasty.TestTree
spec = setTestParams $ Tasty.testGroup "Dijkstra"
    [ Tasty.testGroup "unit tests" $
        assertUnitTestResults (unitTestResults testGraph1)
    , Tasty.testGroup "same result as BellmanFord"
        [ let (edges, expectedList) = testGraph1
          in Tasty.testGroup "unit test" $ -- TODO: get rid of "passed 500 tests"
                expectedList <&> \((src, dst), _) ->
                    TQC.testProperty (src <> " -> " <> dst) $
                        assert_sameResultAsBellmanFord <$> sameResultAsBellmanFordSrcDst dijkstraSourceSinkStr (+) 0 edges ([src], [dst])
        , QS.testPropertyQC "arbitrary graph" $ do
            edges <- arbitraryGraphOld QC.getNonNegative
            assert_sameResultAsBellmanFord <$> sameResultAsBellmanFordAllSrcDst dijkstraSourceSinkStr (+) 0 edges
        ]
    ]
    where
        setTestParams =
            Tasty.localOption (TQC.QuickCheckTests 50) .
            Tasty.localOption (TQC.QuickCheckMaxRatio 100)


        unitTestResults
          :: ( [TestEdge Double]
             , [((String, String), [TestEdge Double])]
             )
          -> [ ( (String, String)
               , ([TestEdge Double], Maybe [TestEdge Double])
               )
             ]
        unitTestResults (edges, expected) = ST.runST $ do
            graph <- Lib.fromEdges edges
            Dijkstra.runDijkstra graph (+) 0 $
                forM expected $ \((source, target), expectedPath) -> do
                    mRes <- dijkstraSourceSinkStr (source, target)
                    pure ((source, target), (expectedPath, map idxEdgeToTestEdge <$> mRes))

        assertUnitTestResults results = do
            results <&> \((source, target), (expectedPath, mPath)) ->
                Test.Tasty.QuickCheck.testProperty (source <> " -> " <> target) $
                    Just expectedPath `shouldBe` mPath

        dijkstraSourceSinkStr (strSrc, strDst) = do
            withVid strSrc $ \vidSrc ->
                withVid strDst $ \vidDst ->
                    fmap fst . listToMaybe <$> Dijkstra.dijkstraKShortestPaths 1 (vidSrc, vidDst)

        withVid str f = do
            g <- Dijkstra.getGraph
            mVid <- R.lift $ Lib.lookupVertex g str
            f $ fromMaybe (error $ "no such vertex ID: " <> show str) mVid

type Result meta = (String, String, Maybe [Lib.IdxEdge String meta])

sameResultAsBellmanFordAllSrcDst
    :: ( v ~ String
       , meta ~ Double
       )
    => (forall s. (v, v) -> Dijkstra.Dijkstra s v meta (Maybe [Lib.IdxEdge v meta]))
    -> (Double -> meta -> Double)
    -> Double
    -> [TestEdge meta]
    -> QC.Gen [([Result meta], [Result meta])]
sameResultAsBellmanFordAllSrcDst dijkstraSpTo combine zero edges =
    let fromTo e = [getFrom e, getTo e]
        vertices = Data.Graph.Util.nubOrd $ concatMap fromTo edges
    in sameResultAsBellmanFordSrcDst dijkstraSpTo combine zero edges (vertices, vertices)

sameResultAsBellmanFordSrcDst
    :: ( v ~ String
       , meta ~ Double
       )
    => (forall s. (v, v) -> Dijkstra.Dijkstra s v meta (Maybe [Lib.IdxEdge v meta]))
    -> (Double -> meta -> Double)
    -> Double
    -> [TestEdge meta]
    -> ([v], [v]) -- (src, dst) pairs to test
    -> QC.Gen [([Result meta], [Result meta])]
sameResultAsBellmanFordSrcDst dijkstraSpTo combine zero edges (srcs, dsts) = do
    shuffledEdges <- QC.shuffle edges
    pure $ ST.runST $ do
        graph <- Lib.fromEdges shuffledEdges
        graphCopy <- copy graph
        forM srcs $ \source -> do
            dijstraPaths <- Dijkstra.runDijkstra graph combine zero $ do
                forM dsts $ \target -> do
                    mRes <- dijkstraSpTo (source, target)
                    pure (source, target, mRes)
            bfPaths <- BellmanFord.runBF graphCopy combine zero $ do
                BellmanFord.bellmanFord source
                forM dsts $ \target -> do
                    mRes <- BellmanFord.pathTo target
                    pure (source, target, mRes)
            pure (dijstraPaths, bfPaths)
    where
        copy g = Lib.freeze g >>= Lib.thaw

assert_sameResultAsBellmanFord
    :: ( v ~ String
       , meta ~ Double
       )
    => [([Result meta], [Result meta])]
    -> QC.Property
assert_sameResultAsBellmanFord results = QC.conjoin $ map QC.conjoin $
    results <&> \(dijstraPaths, bfPaths) ->
        let resultPairs = zip dijstraPaths bfPaths
        in if length resultPairs /= length dijstraPaths
            then error $ "BUG: assert_sameResultAsBellmanFord: non-equal resultPairs length: " <> show (dijstraPaths, bfPaths)
            else resultPairs <&> \(dijkstraPath, bfPath) ->
                let pathWeight' (_, _, mResult) = pathWeight <$> mResult
                    -- We may find two different paths, but the "length" (cumulative weight) of the two paths must be equal (except floating point errors)
                    pathLengths = bimap pathWeight' pathWeight' (dijkstraPath, bfPath)
                    failureMessage = unlines
                        [ unwords
                            [ "Shortest paths not of equal length. Dijkstra length:"
                            , show (pathWeight' dijkstraPath) <> ","
                            , "Bellman-Ford length:"
                            , show $ pathWeight' bfPath
                            ]
                        , displayPath "BellmanFord" bfPath
                        , displayPath "Dijkstra" dijkstraPath
                        ]
                in if | not (uncurry mDoubleEqual pathLengths) ->
                          QC.property $ expectationFailure failureMessage
                      | isUninterestingPath dijkstraPath && isUninterestingPath bfPath && bfPath == dijkstraPath ->
                          QC.discard
                      | otherwise ->
                          QC.property ()
    where
        isUninterestingPath
            :: Result Double -> Bool
        isUninterestingPath (_, _, mPath) = case mPath of
            Nothing -> True
            Just path | length path < 2 -> True
            Just _ -> False

        displayPath name (src, dst, mPath) =
            let mkDescr path = unlines $
                    let fromTo = src <> "->" <> dst
                    in unwords ["Edges on", name, "path (" <> fromTo <> "): "] : map show path
            in maybe "Nothing" mkDescr mPath

        pathWeight = sum . map Lib.eMeta

        mDoubleEqual ma mb =
            case (ma, mb) of
                (Just a, Just b) -> abs (a - b) < epsilon
                (Nothing, Nothing) -> True
                _ -> False

        epsilon :: Double
        epsilon = 1.0e-13
