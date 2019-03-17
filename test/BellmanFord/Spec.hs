{-# LANGUAGE OverloadedStrings #-}
module BellmanFord.Spec
( spec )
where

import           Data.Graph.Prelude
import           BellmanFord.Types                  (TestEdge(..))
import qualified Data.Graph.Digraph                 as Lib
import qualified Data.Graph.BellmanFord             as Lib

import qualified Control.Monad.ST                   as ST
import qualified Test.SmallCheck.Series             as SS
import qualified Test.Hspec.SmallCheck              as SC
import           Test.Hspec                         (Spec, Expectation)
import           Test.Hspec                         (describe, it, parallel)


spec :: Spec
spec =
    describe "bellmanFord" $ do
        it "crashes GHC" $
            SC.property $ do
                graph <- Lib.fromEdges ghcBugData
                bellmanFord graph

bellmanFord
    :: Lib.Digraph ST.RealWorld g TestEdge String
    -> Expectation
bellmanFord graph = do
    vertices <- Lib.vertices graph
    ST.stToIO $ forM_ vertices $ \source -> do
        state <- Lib.bellmanFord graph source
        Lib.check graph state source

-- | Test data that produces
--    "internal error: evacuate: strange closure type" error
ghcBugData =
    [TestEdge {getEdge = ("",""), getWeight = 0.0},TestEdge {getEdge = ("",""), getWeight = 1.0},TestEdge {getEdge = ("",""), getWeight = -1.0},TestEdge {getEdge = ("",""), getWeight = 2.0},TestEdge {getEdge = ("",""), getWeight = 0.5},TestEdge {getEdge = ("",""), getWeight = -2.0},TestEdge {getEdge = ("",""), getWeight = 4.0},TestEdge {getEdge = ("",""), getWeight = 0.25},TestEdge {getEdge = ("",""), getWeight = -0.5},TestEdge {getEdge = ("",""), getWeight = -4.0},TestEdge {getEdge = ("",""), getWeight = -0.25},TestEdge {getEdge = ("a",""), getWeight = 0.0},TestEdge {getEdge = ("a",""), getWeight = 1.0},TestEdge {getEdge = ("a",""), getWeight = -1.0},TestEdge {getEdge = ("a",""), getWeight = 2.0},TestEdge {getEdge = ("a",""), getWeight = 0.5},TestEdge {getEdge = ("a",""), getWeight = -2.0},TestEdge {getEdge = ("a",""), getWeight = 4.0},TestEdge {getEdge = ("a",""), getWeight = 0.25},TestEdge {getEdge = ("a",""), getWeight = -0.5},TestEdge {getEdge = ("a",""), getWeight = -4.0},TestEdge {getEdge = ("a",""), getWeight = -0.25},TestEdge {getEdge = ("","a"), getWeight = 0.0},TestEdge {getEdge = ("","a"), getWeight = 1.0},TestEdge {getEdge = ("","a"), getWeight = -1.0},TestEdge {getEdge = ("","a"), getWeight = 2.0},TestEdge {getEdge = ("","a"), getWeight = 0.5},TestEdge {getEdge = ("","a"), getWeight = -2.0},TestEdge {getEdge = ("","a"), getWeight = 4.0},TestEdge {getEdge = ("","a"), getWeight = 0.25},TestEdge {getEdge = ("","a"), getWeight = -0.5},TestEdge {getEdge = ("","a"), getWeight = -4.0},TestEdge {getEdge = ("","a"), getWeight = -0.25},TestEdge {getEdge = ("b",""), getWeight = 0.0},TestEdge {getEdge = ("b",""), getWeight = 1.0},TestEdge {getEdge = ("b",""), getWeight = -1.0},TestEdge {getEdge = ("b",""), getWeight = 2.0},TestEdge {getEdge = ("b",""), getWeight = 0.5},TestEdge {getEdge = ("b",""), getWeight = -2.0},TestEdge {getEdge = ("b",""), getWeight = 4.0},TestEdge {getEdge = ("b",""), getWeight = 0.25},TestEdge {getEdge = ("b",""), getWeight = -0.5},TestEdge {getEdge = ("b",""), getWeight = -4.0},TestEdge {getEdge = ("b",""), getWeight = -0.25},TestEdge {getEdge = ("","b"), getWeight = 0.0},TestEdge {getEdge = ("","b"), getWeight = 1.0},TestEdge {getEdge = ("","b"), getWeight = -1.0},TestEdge {getEdge = ("","b"), getWeight = 2.0},TestEdge {getEdge = ("","b"), getWeight = 0.5},TestEdge {getEdge = ("","b"), getWeight = -2.0},TestEdge {getEdge = ("","b"), getWeight = 4.0},TestEdge {getEdge = ("","b"), getWeight = 0.25},TestEdge {getEdge = ("","b"), getWeight = -0.5},TestEdge {getEdge = ("","b"), getWeight = -4.0},TestEdge {getEdge = ("","b"), getWeight = -0.25},TestEdge {getEdge = ("a","a"), getWeight = 0.0},TestEdge {getEdge = ("a","a"), getWeight = 1.0},TestEdge {getEdge = ("a","a"), getWeight = -1.0},TestEdge {getEdge = ("a","a"), getWeight = 2.0},TestEdge {getEdge = ("a","a"), getWeight = 0.5},TestEdge {getEdge = ("a","a"), getWeight = -2.0},TestEdge {getEdge = ("a","a"), getWeight = 4.0},TestEdge {getEdge = ("a","a"), getWeight = 0.25},TestEdge {getEdge = ("a","a"), getWeight = -0.5},TestEdge {getEdge = ("a","a"), getWeight = -4.0},TestEdge {getEdge = ("a","a"), getWeight = -0.25},TestEdge {getEdge = ("b","a"), getWeight = 0.0},TestEdge {getEdge = ("b","a"), getWeight = 1.0},TestEdge {getEdge = ("b","a"), getWeight = -1.0},TestEdge {getEdge = ("b","a"), getWeight = 2.0},TestEdge {getEdge = ("b","a"), getWeight = 0.5},TestEdge {getEdge = ("b","a"), getWeight = -2.0},TestEdge {getEdge = ("b","a"), getWeight = 4.0},TestEdge {getEdge = ("b","a"), getWeight = 0.25},TestEdge {getEdge = ("b","a"), getWeight = -0.5},TestEdge {getEdge = ("b","a"), getWeight = -4.0},TestEdge {getEdge = ("b","a"), getWeight = -0.25},TestEdge {getEdge = ("a","b"), getWeight = 0.0},TestEdge {getEdge = ("a","b"), getWeight = 1.0},TestEdge {getEdge = ("a","b"), getWeight = -1.0},TestEdge {getEdge = ("a","b"), getWeight = 2.0},TestEdge {getEdge = ("a","b"), getWeight = 0.5},TestEdge {getEdge = ("a","b"), getWeight = -2.0},TestEdge {getEdge = ("a","b"), getWeight = 4.0},TestEdge {getEdge = ("a","b"), getWeight = 0.25},TestEdge {getEdge = ("a","b"), getWeight = -0.5},TestEdge {getEdge = ("a","b"), getWeight = -4.0},TestEdge {getEdge = ("a","b"), getWeight = -0.25},TestEdge {getEdge = ("b","b"), getWeight = 0.0},TestEdge {getEdge = ("b","b"), getWeight = 1.0},TestEdge {getEdge = ("b","b"), getWeight = -1.0},TestEdge {getEdge = ("b","b"), getWeight = 2.0},TestEdge {getEdge = ("b","b"), getWeight = 0.5},TestEdge {getEdge = ("b","b"), getWeight = -2.0},TestEdge {getEdge = ("b","b"), getWeight = 4.0},TestEdge {getEdge = ("b","b"), getWeight = 0.25},TestEdge {getEdge = ("b","b"), getWeight = -0.5},TestEdge {getEdge = ("b","b"), getWeight = -4.0},TestEdge {getEdge = ("b","b"), getWeight = -0.25}]
