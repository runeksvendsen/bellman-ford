{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
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

-- | @((srcNode, dstNode), metadata)@
instance (Eq nodeLabel, Hashable nodeLabel) =>
  DirectedEdge ((nodeLabel, nodeLabel), meta) nodeLabel meta where
    fromNode = fst . fst
    toNode = snd . fst
    metaData = snd
