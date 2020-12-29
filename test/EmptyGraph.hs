module EmptyGraph
( removePaths
)
where

import Types.Edge

import Data.Graph.Prelude (when, forM_)
import qualified Data.Graph.Digraph                 as Lib
import qualified Data.Graph.BellmanFord             as Lib

import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (MonadTrans(lift))


removePaths
    :: [TestEdge]
    -> String -- ^ End vertex
    -> ST s ()
removePaths edges' end = do
    graph <- Lib.fromEdges edges'
    vs <- Lib.vertexLabels graph
    Lib.runBF graph sumWeight 0 $ forM_ vs (go graph Lib.negativeCycle)
    Lib.runBF graph sumWeight 0 $ forM_ vs $ \v -> do
        when (v /= end) $ go graph (Lib.pathTo end) v
  where
    sumWeight weight' edge = weight' + Lib.weight edge
    go graph action v = do
        Lib.bellmanFord v
        edgesM <- action
        case edgesM of
            Nothing -> return ()
            Just edges -> do
                mapM_ (lift . Lib.removeEdge graph) edges
                go graph action v
