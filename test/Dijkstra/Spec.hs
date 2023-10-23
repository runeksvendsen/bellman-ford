{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Dijkstra.Spec
( spec
)
where

import           Data.Graph.Prelude
import           Types.Edge
import           Types.Graph
import qualified Data.Graph.Digraph                 as Lib
import qualified Data.Graph.BellmanFord             as BellmanFord
import qualified Data.Graph.Dijkstra                as Dijkstra

import qualified Control.Monad.ST                   as ST
import qualified Test.Hspec.SmallCheck              ()
import           Test.Hspec.Expectations.Pretty
import qualified Test.Tasty                         as Tasty
import qualified Test.QuickCheck as QC
import qualified Util.QuickSmall as QS
import Data.Bifunctor (bimap)


spec :: Tasty.TestTree
spec = Tasty.testGroup "Dijkstra"
    [ Tasty.testGroup "same result as BellmanFord"
        [ QS.testPropertyQC "for arbitrary edge list" $ \edges ->
            sameResultAsBellmanFord (+) 0 (map nonNegativeWeight edges)
        , QS.testPropertyQC "for arbitrary graph" $ do
            edges <- arbitraryGraph QC.getNonNegative
            sameResultAsBellmanFord (+) 0 edges
        ]
    ]

sameResultAsBellmanFord
    :: (Double -> Double -> Double)
    -> Double
    -> [TestEdge]
    -> QC.Gen QC.Property
sameResultAsBellmanFord combine zero edges = do
    shuffledEdges <- QC.shuffle edges
    let result = ST.runST $ do
            graph <- Lib.fromEdges shuffledEdges
            graphCopy <- copy graph
            vertices <- Lib.vertexLabels graph
            forM vertices $ \source -> do
                dijstraPaths <- allShortestPathsFromSource
                    graph source vertices Dijkstra.runDijkstra Dijkstra.dijkstra Dijkstra.pathTo
                bfPaths <- allShortestPathsFromSource
                    graphCopy source vertices BellmanFord.runBF BellmanFord.bellmanFord BellmanFord.pathTo
                pure (dijstraPaths, bfPaths)
    pure $ QC.property $ forM_ result $ \(dijstraPaths, bfPaths) -> do
        let resultPairs = zip dijstraPaths bfPaths
        unless (length resultPairs == length dijstraPaths) $
            fail $ "BUG: sameResultAsBellmanFord: non-equal resultPairs length: " <> show (dijstraPaths, bfPaths)
        forM resultPairs $ \(dijstraPath, bfPath) -> do
            let pathWeight' mPath =
                    let getPath (_, _, edgeList) = edgeList
                    in pathWeight . getPath <$> mPath
            -- We may find two different paths, but the "length" (cumulative weight) of the two paths must be equal (except floating point errors)
            let pathLengths = bimap pathWeight' pathWeight' $ (dijstraPath, bfPath)
            unless (uncurry mDoubleEqual pathLengths) $
                expectationFailure $ unlines
                    [ unwords ["Shortest paths not of equal length. Length:", show pathLengths]
                    , displayPath "BellmanFord" bfPath
                    , displayPath "Dijkstra" dijstraPath
                    ]
    where
        displayPath name mPath =
            maybe "Nothing" (\(src, dst, path) -> unlines $ (name <> ": " <> src <> "->" <> dst) : (map show path)) mPath

        copy g = Lib.freeze g >>= Lib.thaw

        pathWeight = sum . map Lib.eMeta

        mDoubleEqual ma mb =
            case (ma, mb) of
                (Just a, Just b) -> abs (a - b) < epsilon
                (Nothing, Nothing) -> True
                _ -> False

        epsilon :: Double
        epsilon = 1.0e-13

        allShortestPathsFromSource
            :: Monad m
            => Lib.Digraph s String Double
            -> String
            -> [String]
            -> (forall a.
                   Lib.Digraph s String Double
                -> (Double -> Double -> Double)
                -> Double
                -> m a
                -> ST s a
               )
            -> (String -> m ())
            -> (   String
                -> m (Maybe [Lib.IdxEdge String Double])
               )
            -> ST s [Maybe (String, String, [Lib.IdxEdge String Double])]
        allShortestPathsFromSource graph source vertices runner findShortestPaths pathTo =
            runner graph (\weight edge -> weight `combine` Lib.weight edge) zero $ do
                findShortestPaths source
                forM vertices $ \target -> do
                    mRes <- pathTo target
                    forM mRes $ \res -> pure (source, target, res)
