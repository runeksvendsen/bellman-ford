{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BellmanFord.Spec
( spec
)
where

import           Data.Graph.Prelude
import           BellmanFord.Types                  (TestEdge, PositiveWeight(..))
import qualified Data.Graph.Digraph                 as Lib
import qualified Data.Graph.BellmanFord             as Lib

import qualified Control.Monad.ST                   as ST
import qualified Test.Hspec.SmallCheck              ()
import           Test.Hspec                         (Expectation)
import qualified Test.Tasty                         as Tasty
import           Test.Tasty.SmallCheck              as SC


spec :: Tasty.TestTree
spec = Tasty.testGroup "BellmanFord" $
    [ Tasty.testGroup "additive (all weights)"
        [ SC.testProperty "passes 'check'" $ bellmanFord (+)
        ]
    , Tasty.testGroup "multiplicative (positive weights)"
        [ SC.testProperty "passes 'check'" $ bellmanFord (*) . map positiveWeight
        ]
    ]

bellmanFord
    :: (Double -> Double -> Double)
    -> [TestEdge]
    -> Expectation
bellmanFord combine edges = do
    graph <- Lib.fromEdges edges
    vertices <- Lib.vertices graph
    ST.stToIO $ forM_ vertices $ \source -> do
        Lib.bellmanFord graph source (\weight edge -> weight `combine` Lib.weight edge)
