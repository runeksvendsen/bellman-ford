{-# LANGUAGE OverloadedStrings #-}
module BellmanFord.Spec
( spec
, tastyProps
)
where

import           Data.Graph.Prelude
import           BellmanFord.Types                  (TestEdge)
import qualified Data.Graph.Digraph                 as Lib
import qualified Data.Graph.BellmanFord             as Lib

import qualified Control.Monad.ST                   as ST
import qualified Test.SmallCheck.Series             as SS
import qualified Test.Hspec.SmallCheck              as SC
import           Test.Hspec                         (Spec, Expectation)
import           Test.Hspec                         (describe, it, parallel)
-- TASTY
import Test.Tasty
import Test.Tasty.SmallCheck  as SC


tastyProps = testGroup "bellmanFord"
  [ SC.testProperty "passes 'check'" $ bellmanFord
  ]


spec :: Spec
spec = parallel $ do
    describe "bellmanFord" $ do
        it "passes 'check'" $
            SC.property bellmanFord

bellmanFord
    :: Lib.Digraph ST.RealWorld g TestEdge String
    -> Expectation
bellmanFord graph = do
    vertices <- Lib.vertices graph
    ST.stToIO $ forM_ vertices $ \source -> do
        Lib.bellmanFord graph source (\weight edge -> weight + Lib.weight edge)
