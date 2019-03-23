{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BellmanFord.Spec
( spec
, tastyProps
)
where

import           Data.Graph.Prelude
import           BellmanFord.Types                  (TestEdge, PositiveWeight(..))
import qualified Data.Graph.Digraph                 as Lib
import qualified Data.Graph.BellmanFord             as Lib

import qualified Control.Monad.ST                   as ST
import qualified Test.Hspec.SmallCheck              as SC
import           Test.Hspec                         (Spec, Expectation)
import           Test.Hspec                         (describe, it, parallel)
-- TASTY
import Test.Tasty
import Test.Tasty.SmallCheck  as SC


tastyProps :: [TestTree]
tastyProps =
    [ testGroup "additive (all weights)"
        [ SC.testProperty "passes 'check'" $ bellmanFord (+)
        ]
    , testGroup "multiplicative (positive weights)"
        [ SC.testProperty "passes 'check'" $ bellmanFord (*) . map positiveWeight
        ]
    ]

spec :: Spec
spec = parallel $ do
    describe "bellmanFord additive (all weights)" $ do
        it "passes 'check'" $
            SC.property $ bellmanFord (+)
    describe "bellmanFord multiplicative (positive weights)" $ do
        it "passes 'check'" $
            SC.property $ bellmanFord (*) . map positiveWeight

bellmanFord
    :: (Double -> Double -> Double)
    -> [TestEdge]
    -> Expectation
bellmanFord combine edges = do
    graph <- Lib.fromEdges edges
    vertices <- Lib.vertices graph
    ST.stToIO $ forM_ vertices $ \source -> do
        Lib.bellmanFord graph source (\weight edge -> weight `combine` Lib.weight edge)
