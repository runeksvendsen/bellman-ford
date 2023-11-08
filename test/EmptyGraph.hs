module EmptyGraph
( removePaths
)
where

import Data.Graph.Prelude (when, forM_)
import qualified Data.Graph.Digraph                 as Lib
import qualified Data.Graph.BellmanFord.Unboxed     as Lib

import Control.Monad.ST (ST)
import Control.Monad.Trans.Class (MonadTrans(lift))

-- | (1) Run Bellman-Ford
--   (2) If either a shortest path or a negative-weight cycle is found:
--     (2a) Remove the edges comprising this path from the graph
--     (2b) Go to (1)
removePaths
    :: (Lib.Unboxable weight s, Show weight, Eq weight, Num weight, Show meta, Eq meta)
    => Lib.Digraph s String meta
    -> String -- ^ End vertex
    -> ST s (Lib.BF s String weight meta ())
removePaths graph end = do
    vs <- Lib.vertexLabels graph
    pure $ do
        forM_ vs (go Lib.negativeCycle)
        forM_ vs $ \v -> do
            when (v /= end) $ go (Lib.pathTo end) v
  where
    go action v = do
        Lib.bellmanFord v
        edgesM <- action
        case edgesM of
            Nothing -> return ()
            Just edges -> do
                mapM_ (lift . Lib.removeEdge graph) edges
                go action v
