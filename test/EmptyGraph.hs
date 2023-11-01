module EmptyGraph
( removePaths
)
where

import Types.Edge

import Data.Graph.Prelude (when, forM_)
import qualified Data.Graph.Digraph                 as Lib
import qualified Data.Graph.BellmanFord.Unboxed     as Lib

import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (MonadTrans(lift))


removePaths
    :: (Lib.Unboxable weight s, Show weight, Eq weight, Num weight)
    => (weight -> weight -> Bool) -- ^ @isLessThan@ function
    -> weight -- ^ "Zero" element
    -> weight -- ^ "Infinity" element
    -> [TestEdge weight]
    -> String -- ^ End vertex
    -> ST s ()
removePaths isLessThan zero infinity edges' end = do
    graph <- Lib.fromEdges edges'
    vs <- Lib.vertexLabels graph
    Lib.runBF graph sumWeight isLessThan zero infinity $ forM_ vs (go graph Lib.negativeCycle)
    Lib.runBF graph sumWeight isLessThan zero infinity $ forM_ vs $ \v -> do
        when (v /= end) $ go graph (Lib.pathTo end) v
  where
    sumWeight = (+)

    go graph action v = do
        Lib.bellmanFord v
        edgesM <- action
        case edgesM of
            Nothing -> return ()
            Just edges -> do
                mapM_ (lift . Lib.removeEdge graph) edges
                go graph action v
