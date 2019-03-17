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


edges = [ TestEdge ("LOL","WAT") (-47)
        ]

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
