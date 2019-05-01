module Data.Graph.Util
( lookupVertex
) where

import           Prelude
import           Data.Graph.Prelude
import qualified Data.Graph.Digraph                 as DG
import qualified Data.Graph.Types.Internal          as GI


lookupVertex
    :: (PrimMonad m, Eq v, Hashable v, Show v)
    => DG.Digraph (PrimState m) g e v
    -> v
    -> m (GI.Vertex g)
lookupVertex graph v =
    fromMaybe (error $ "Vertex not found: " ++ show v) <$> DG.lookupVertex graph v
