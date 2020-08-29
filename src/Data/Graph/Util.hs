module Data.Graph.Util
( lookupVertex
) where

import           Prelude
import           Data.Graph.Prelude
import qualified Data.Graph.Digraph                 as DG


lookupVertex
    :: (Eq v, Hashable v, Show v)
    => DG.Digraph s v ek e
    -> v
    -> ST s (DG.Vertex g)
lookupVertex graph v =
    fromMaybe (error $ "Vertex not found: " ++ show v) <$> DG.lookupVertex graph v
