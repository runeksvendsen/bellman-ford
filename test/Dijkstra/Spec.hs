{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
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
import           Test.Hspec.Expectations.Pretty            ( shouldBe
                                                    )
import qualified Test.Tasty                         as Tasty
import qualified Test.QuickCheck as QC
import qualified Util.QuickSmall as QS


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
    :: (Lib.HasWeight a Double, Show a, Show v, Lib.DirectedEdge edge v a, Ord v, Eq a)
    => (Double -> Double -> Double)
    -> Double
    -> [edge]
    -> QC.Gen QC.Property
sameResultAsBellmanFord combine zero edges = do
    shuffledEdges <- QC.shuffle edges
    let result = ST.runST $ do
            graph <- Lib.fromEdges shuffledEdges
            vertices <- Lib.vertexLabels graph
            forM vertices $ \source -> do
                dijstraPaths <- Dijkstra.runDijkstra graph (\weight edge -> weight `combine` Lib.weight edge) zero $ do
                    Dijkstra.dijkstra source
                    forM vertices $ \target -> do
                        mRes <- Dijkstra.pathTo target
                        forM mRes $ \res -> pure (source, target, res)
                graph2 <- copy graph
                bfPaths <- BellmanFord.runBF graph2 (\weight edge -> weight `combine` Lib.weight edge) zero $ do
                    BellmanFord.bellmanFord source
                    forM vertices $ \target -> do
                        mRes <- BellmanFord.pathTo target
                        forM mRes $ \res -> pure (source, target, res)
                pure (dijstraPaths, bfPaths)
    pure $ QC.property $ forM_ result $ \(dijstraPaths, bfPaths) -> do
        let resultPairs = zip dijstraPaths bfPaths
        unless (length resultPairs == length dijstraPaths) $
            fail $ "BUG: sameResultAsBellmanFord: non-equal resultPairs length: " <> show (dijstraPaths, bfPaths)
        dijstraPaths `shouldBe` bfPaths
    where
        copy g = Lib.freeze g >>= Lib.thaw
