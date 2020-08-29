{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Graph.Edge
( WeightedEdge(..)
, DirectedEdge(..)
)
where

import           Data.Hashable                  (Hashable)
import qualified Data.Text  as T

-- | An edge in a graph
class ( Eq nodeLabel
      , Hashable nodeLabel
      ) => DirectedEdge edge nodeLabel | edge -> nodeLabel where
    fromNode :: edge -> nodeLabel   -- ^ Label associated with the edge's "from" node
    toNode   :: edge -> nodeLabel   -- ^ Label associated with the edge's "to" node
    multiKey :: edge -> T.Text      -- ^ Temporary hack

-- | A weighted edge
class ( DirectedEdge edge nodeLabel
      ) => WeightedEdge edge nodeLabel weightType | edge -> nodeLabel weightType where
    weight   :: edge -> weightType    -- ^ Edge's weight
