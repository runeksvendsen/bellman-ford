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
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Test.Hspec.Expectations
import qualified Test.Tasty                         as Tasty
import qualified Test.QuickCheck as QC
import Data.Bifunctor (bimap, first)
import Data.Functor ((<&>))
import qualified Test.Tasty.QuickCheck
import qualified Test.Tasty.QuickCheck as TQC
import qualified Control.Concurrent.Chan as Chan
import qualified Data.Time
import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Proxy (Proxy)
import Data.Data (Proxy(Proxy))
import Data.Fixed (Pico)
import qualified System.Timeout
import qualified Control.DeepSeq
import qualified Control.Exception
import Debug.Trace (trace)

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
spec = setNumTestsAndMaxRatio 2000 3 $ Tasty.testGroup "Dijkstra"
    [ Tasty.testGroup "unit tests" $
        assertUnitTestResults (unitTestResults testGraph1)
    , Tasty.testGroup "same result as BellmanFord"
        [ setNumTestsAndMaxRatio 1 1 $
          let (edges, expectedList) = testGraph1
          in Tasty.testGroup "unit test" $ -- TODO: get rid of "passed 500 tests"
                expectedList <&> \((src, dst), _) ->
                    TQC.testProperty (src <> " -> " <> dst) $
                        assert_sameResultAsBellmanFord <$> sameResultAsBellmanFordSrcDst dijkstraSourceSinkStr (+) 0 edges ([src], [dst])
        , Tasty.testGroup "arbitrary graph"
            [ TQC.testProperty "arbitraryGraphEdges" $ do
               edges <- map (fmap QC.getNonNegative) <$> arbitraryGraphEdges
               assert_sameResultAsBellmanFord <$> sameResultAsBellmanFordAllSrcDst' edges
            , TQC.testProperty "arbitraryConnectedGraph" $ do
               edges <- map (fmap QC.getNonNegative) <$> arbitraryConnectedGraph 2
               assert_sameResultAsBellmanFord <$> sameResultAsBellmanFordAllSrcDst' edges
            ]
        ]
    , setNumTests 10000 $ Tasty.testGroup "dijkstraShortestPathsLevelsTimeout returns subset of dijkstraShortestPathsLevels"
        [ TQC.testProperty "GraphEdges" $ \graph args -> do
            let edges = map (fmap QC.getNonNegative) $ unGraphEdges
                    (graph :: GraphEdges (QC.NonNegative Double))
            test_dijkstraShortestPathsLevelsTimeout edges args
        , TQC.testProperty "ConnectedGraph" $ \graph args -> do
            let edges = map (fmap QC.getNonNegative) $ unConnectedGraph
                    (graph :: ConnectedGraph 2 (QC.NonNegative Double))
            test_dijkstraShortestPathsLevelsTimeout edges args
        ]
    ]
    where
        sameResultAsBellmanFordAllSrcDst' =
            sameResultAsBellmanFordAllSrcDst dijkstraSourceSinkStr (+) 0

        setNumTestsAndMaxRatio numTests maxRatio =
            setNumTests numTests .
            Tasty.localOption (TQC.QuickCheckMaxRatio maxRatio)

        setNumTests numTests =
            Tasty.localOption (TQC.QuickCheckTests numTests)

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
    let vertices = edgeListVertices edges
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
assert_sameResultAsBellmanFord results = handleResults $ concat $
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
                          Just $ QC.property $ expectationFailure failureMessage
                      | isUninterestingPath dijkstraPath && isUninterestingPath bfPath && bfPath == dijkstraPath ->
                          Nothing
                      | otherwise ->
                          Just $ QC.property ()
    where
        handleResults res =
            case catMaybes res of
                [] -> QC.discard -- all results were discarded
                nonDiscards -> QC.conjoin nonDiscards -- not all results were discarded

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

test_dijkstraShortestPathsLevelsTimeout
    :: [TestEdge Double]
    -> ShortestPathsLevelsArgs 1 10000 -- 1μs to 10ms
    -> TQC.Property
test_dijkstraShortestPathsLevelsTimeout [] _ = QC.discard
test_dijkstraShortestPathsLevelsTimeout edges ShortestPathsLevelsArgs{..} =
    QC.forAll srcDstGen $ \srcDst ->
        TQC.within 5e6 $ -- TODO: add NOTE: should not be triggered
            QC.ioProperty $
                assertResults <$> genResults srcDst

    where
        assertResults (results, (timeoutResults, timedOut)) = do
            let assertPathFunction =
                    if timedOut
                        then Test.Hspec.Expectations.shouldStartWith
                        else Test.Hspec.Expectations.shouldBe
                labelStr = "timeout: " <> show labelStr
            results `assertPathFunction` reverse timeoutResults -- WIP: why reverse?

        genResults (srcLabel, dstLabel) = do
            (graph, srcDst) <- stToIO $ do
                graph <- Lib.fromEdges edges
                src <- lookupVertex graph srcLabel
                dst <- lookupVertex graph dstLabel
                let srcDst = (src, dst)
                pure (graph, srcDst)
            -- TODO: in parallel?
            results <- timeoutFail "dijkstraShortestPathsLevels" 1 $
                (Control.Exception.evaluate . Control.DeepSeq.force) =<<
                stToIO
                    (runner graph $
                        map getResult <$> Dijkstra.dijkstraShortestPathsLevels k numLevels srcDst)
            timeoutResTimeBoundedResult <- timeoutFail "dijkstraShortestPathsLevelsTimeout" 1 $ -- (timeout * 2) $
                Dijkstra.dijkstraShortestPathsLevelsTimeout
                    (runner graph)
                    k
                    numLevels
                    srcDst
                    timeout
                    getChanContents
            print timeoutResTimeBoundedResult
            pure ( results
                 , let res = extractResults $ map (fmap getResult) timeoutResTimeBoundedResult
                   in show res `trace` res
                 )

        timeoutFail actionName timeout' action =
            let micros = ceiling $ Data.Time.nominalDiffTimeToSeconds timeout' * 1e6
            in System.Timeout.timeout micros action >>= \case
                Nothing -> fail $ actionName <> " timed out after " <> show timeout'
                Just a -> pure a

        runner graph = Dijkstra.runDijkstra graph (+) 0

        vertices = edgeListVertices edges
        srcDstGen = (,) <$> QC.elements vertices <*> QC.elements vertices

        lookupVertex graph str =
            Lib.lookupVertex graph str >>=
            maybe
                (fail $ "test_dijkstraShortestPathsLevelsTimeout: BUG: vertex not found")
                pure

        getResult :: ([Lib.IdxEdge String meta], c) -> ([TestEdge meta], c)
        getResult = first (map idxEdgeToTestEdge)

        getChanContents
            :: Chan.Chan (Dijkstra.TimeBoundedResult a)
            -> IO [Dijkstra.TimeBoundedResult a]
        getChanContents chan =
            go
              where
                go = Chan.readChan chan >>= \case
                        res@Dijkstra.TimeBoundedResult_Result{} -> (res :) <$> go
                        res -> pure [res]

        -- also asserts that only the last element of the list is either 'Done' or 'TimedOut';
        -- and that all other elements are 'Result'.
        extractResults
            :: Show a
            => [Dijkstra.TimeBoundedResult a]
            -> ([a], Bool)
            -- Bool: timed out?
        extractResults [] = error "extractResults: empty list"
        extractResults nonEmptyList =
            let getTimeBoundedResultItem (Dijkstra.TimeBoundedResult_Result item) = item
                getTimeBoundedResultItem other = error $ "extractResults: unexpected element " <> show other <> ". " <> show nonEmptyList
                timeBoundedResults = map getTimeBoundedResultItem (init nonEmptyList)
            in case last nonEmptyList of
                Dijkstra.TimeBoundedResult_Result{} ->
                    error $ "extractResults: last element was 'Result': " <> show nonEmptyList
                Dijkstra.TimeBoundedResult_Done -> (timeBoundedResults, False)
                Dijkstra.TimeBoundedResult_TimedOut -> (timeBoundedResults, True)

-- | Arguments to 'Dijkstra.dijkstraShortestPathsLevels'
data ShortestPathsLevelsArgs (minTimeRangeMicros :: Nat) (maxTimeRangeMicros :: Nat) = ShortestPathsLevelsArgs
    { timeout :: !Data.Time.NominalDiffTime
    , k :: !Int
    , numLevels :: !Int
    } deriving (Show)

instance (KnownNat minTimeoutMicros, KnownNat maxTimeoutMicros)
    => QC.Arbitrary (ShortestPathsLevelsArgs minTimeoutMicros maxTimeoutMicros) where
        arbitrary = do
            let timeoutRangeSeconds :: (Pico, Pico)
                timeoutRangeSeconds =
                    ( (/ 1e6) $ fromIntegral $ natVal (Proxy :: Proxy minTimeoutMicros)
                    , (/ 1e6) $ fromIntegral $ natVal (Proxy :: Proxy maxTimeoutMicros)
                    )
            timeout' <- Data.Time.secondsToNominalDiffTime <$> QC.chooseEnum timeoutRangeSeconds
            k' <- QC.arbitrary
            numLevels' <- QC.arbitrary
            pure $ ShortestPathsLevelsArgs
                { timeout = timeout'
                , k = k'
                , numLevels = numLevels'
                }
