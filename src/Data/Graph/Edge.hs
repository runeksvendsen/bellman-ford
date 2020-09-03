{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Graph.Edge
( DirectedEdge(..)
)
where

import           Data.Hashable                  (Hashable)

-- | An edge in a graph
class ( Eq nodeLabel
      , Hashable nodeLabel
      ) => DirectedEdge edge nodeLabel meta | edge -> nodeLabel meta where
    fromNode :: edge -> nodeLabel   -- ^ Label associated with the edge's "from" node
    toNode   :: edge -> nodeLabel   -- ^ Label associated with the edge's "to" node
    metaData :: edge -> meta
