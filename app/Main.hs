{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import           Data.Graph.Prelude
import qualified Data.Graph.Digraph                 as Lib
import           Data.Graph.Edge                    as Lib
import qualified Data.Graph.BellmanFord             as Lib
import           Data.Graph.Types.Internal          (Vertex(getVertexInternal))

import qualified Control.Monad.ST                   as ST
import qualified Control.Monad.ST.Unsafe            as Unsafe


edges = [TestEdge {getEdge = ("",""), getWeight = 0.0},TestEdge {getEdge = ("",""), getWeight = 1.0},TestEdge {getEdge = ("",""), getWeight = -1.0},TestEdge {getEdge = ("",""), getWeight = 2.0},TestEdge {getEdge = ("",""), getWeight = 0.5},TestEdge {getEdge = ("",""), getWeight = -2.0},TestEdge {getEdge = ("",""), getWeight = 4.0},TestEdge {getEdge = ("",""), getWeight = 0.25},TestEdge {getEdge = ("",""), getWeight = -0.5},TestEdge {getEdge = ("",""), getWeight = -4.0},TestEdge {getEdge = ("",""), getWeight = -0.25},TestEdge {getEdge = ("a",""), getWeight = 0.0},TestEdge {getEdge = ("a",""), getWeight = 1.0},TestEdge {getEdge = ("a",""), getWeight = -1.0},TestEdge {getEdge = ("a",""), getWeight = 2.0},TestEdge {getEdge = ("a",""), getWeight = 0.5},TestEdge {getEdge = ("a",""), getWeight = -2.0},TestEdge {getEdge = ("a",""), getWeight = 4.0},TestEdge {getEdge = ("a",""), getWeight = 0.25},TestEdge {getEdge = ("a",""), getWeight = -0.5},TestEdge {getEdge = ("a",""), getWeight = -4.0},TestEdge {getEdge = ("a",""), getWeight = -0.25},TestEdge {getEdge = ("","a"), getWeight = 0.0},TestEdge {getEdge = ("","a"), getWeight = 1.0},TestEdge {getEdge = ("","a"), getWeight = -1.0},TestEdge {getEdge = ("","a"), getWeight = 2.0},TestEdge {getEdge = ("","a"), getWeight = 0.5},TestEdge {getEdge = ("","a"), getWeight = -2.0},TestEdge {getEdge = ("","a"), getWeight = 4.0},TestEdge {getEdge = ("","a"), getWeight = 0.25},TestEdge {getEdge = ("","a"), getWeight = -0.5},TestEdge {getEdge = ("","a"), getWeight = -4.0},TestEdge {getEdge = ("","a"), getWeight = -0.25},TestEdge {getEdge = ("b",""), getWeight = 0.0},TestEdge {getEdge = ("b",""), getWeight = 1.0},TestEdge {getEdge = ("b",""), getWeight = -1.0},TestEdge {getEdge = ("b",""), getWeight = 2.0},TestEdge {getEdge = ("b",""), getWeight = 0.5},TestEdge {getEdge = ("b",""), getWeight = -2.0},TestEdge {getEdge = ("b",""), getWeight = 4.0},TestEdge {getEdge = ("b",""), getWeight = 0.25},TestEdge {getEdge = ("b",""), getWeight = -0.5},TestEdge {getEdge = ("b",""), getWeight = -4.0},TestEdge {getEdge = ("b",""), getWeight = -0.25},TestEdge {getEdge = ("","b"), getWeight = 0.0},TestEdge {getEdge = ("","b"), getWeight = 1.0},TestEdge {getEdge = ("","b"), getWeight = -1.0},TestEdge {getEdge = ("","b"), getWeight = 2.0},TestEdge {getEdge = ("","b"), getWeight = 0.5},TestEdge {getEdge = ("","b"), getWeight = -2.0},TestEdge {getEdge = ("","b"), getWeight = 4.0},TestEdge {getEdge = ("","b"), getWeight = 0.25},TestEdge {getEdge = ("","b"), getWeight = -0.5},TestEdge {getEdge = ("","b"), getWeight = -4.0},TestEdge {getEdge = ("","b"), getWeight = -0.25},TestEdge {getEdge = ("a","a"), getWeight = 0.0},TestEdge {getEdge = ("a","a"), getWeight = 1.0},TestEdge {getEdge = ("a","a"), getWeight = -1.0},TestEdge {getEdge = ("a","a"), getWeight = 2.0},TestEdge {getEdge = ("a","a"), getWeight = 0.5},TestEdge {getEdge = ("a","a"), getWeight = -2.0},TestEdge {getEdge = ("a","a"), getWeight = 4.0},TestEdge {getEdge = ("a","a"), getWeight = 0.25},TestEdge {getEdge = ("a","a"), getWeight = -0.5},TestEdge {getEdge = ("a","a"), getWeight = -4.0},TestEdge {getEdge = ("a","a"), getWeight = -0.25},TestEdge {getEdge = ("b","a"), getWeight = 0.0},TestEdge {getEdge = ("b","a"), getWeight = 1.0},TestEdge {getEdge = ("b","a"), getWeight = -1.0},TestEdge {getEdge = ("b","a"), getWeight = 2.0},TestEdge {getEdge = ("b","a"), getWeight = 0.5},TestEdge {getEdge = ("b","a"), getWeight = -2.0},TestEdge {getEdge = ("b","a"), getWeight = 4.0},TestEdge {getEdge = ("b","a"), getWeight = 0.25},TestEdge {getEdge = ("b","a"), getWeight = -0.5},TestEdge {getEdge = ("b","a"), getWeight = -4.0},TestEdge {getEdge = ("b","a"), getWeight = -0.25},TestEdge {getEdge = ("a","b"), getWeight = 0.0},TestEdge {getEdge = ("a","b"), getWeight = 1.0},TestEdge {getEdge = ("a","b"), getWeight = -1.0},TestEdge {getEdge = ("a","b"), getWeight = 2.0},TestEdge {getEdge = ("a","b"), getWeight = 0.5},TestEdge {getEdge = ("a","b"), getWeight = -2.0},TestEdge {getEdge = ("a","b"), getWeight = 4.0},TestEdge {getEdge = ("a","b"), getWeight = 0.25},TestEdge {getEdge = ("a","b"), getWeight = -0.5},TestEdge {getEdge = ("a","b"), getWeight = -4.0},TestEdge {getEdge = ("a","b"), getWeight = -0.25},TestEdge {getEdge = ("b","b"), getWeight = 0.0},TestEdge {getEdge = ("b","b"), getWeight = 1.0},TestEdge {getEdge = ("b","b"), getWeight = -1.0},TestEdge {getEdge = ("b","b"), getWeight = 2.0},TestEdge {getEdge = ("b","b"), getWeight = 0.5},TestEdge {getEdge = ("b","b"), getWeight = -2.0},TestEdge {getEdge = ("b","b"), getWeight = 4.0},TestEdge {getEdge = ("b","b"), getWeight = 0.25},TestEdge {getEdge = ("b","b"), getWeight = -0.5},TestEdge {getEdge = ("b","b"), getWeight = -4.0},TestEdge {getEdge = ("b","b"), getWeight = -0.25}]

main :: IO ()
main = do
    graph <- Lib.fromEdges edges
    bellmanFord graph

bellmanFord
    :: Lib.Digraph ST.RealWorld g TestEdge String
    -> IO ()
bellmanFord graph = do
    vertices <- Lib.vertices graph
    ST.stToIO $ forM_ vertices $ \source -> do
        Unsafe.unsafeIOToST $ putStrLn $ "Vertex: " ++ show (getVertexInternal source)
        state <- Lib.bellmanFord graph source
        Lib.check graph state source

data TestEdge = TestEdge
    { getEdge     :: (String, String)
    , getWeight   :: Double
    } deriving (Eq, Show)

instance Lib.DirectedEdge TestEdge String where
   fromNode = fst . getEdge
   toNode = snd . getEdge

instance Lib.WeightedEdge TestEdge String Double where
   weight = getWeight
