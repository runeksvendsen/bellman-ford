{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Graph.IdxEdge
( IdxEdge(..)
, VertexId(..)
  -- NB: Export setter for the 'eMeta' record field
  --  but only getters for the remaining record fields
, eMeta
, eFrom
, eTo
, eFromIdx
, eToIdx
, HasWeight(..)
  -- * Re-exports
, DirectedEdge(..)
)
where

import Data.Graph.Prelude
import Data.Graph.Edge (DirectedEdge(..))
import Data.Ix (Ix(..))

data IdxEdge v meta = IdxEdge
    { eMeta     :: !meta
    , _eFrom    :: !v
    , _eTo      :: !v
    , _eFromIdx :: {-# UNPACK #-} !VertexId
    , _eToIdx   :: {-# UNPACK #-} !VertexId
    }

instance (Eq v, Hashable v) => DirectedEdge (IdxEdge v meta) v meta where
   fromNode = _eFrom
   toNode = _eTo
   metaData = eMeta

class HasWeight a weight | a -> weight where
    weight :: a -> weight

newtype VertexId = VertexId Int
    deriving (Eq, Ord, Hashable, Ix)

eFrom :: IdxEdge v meta -> v
eFrom = _eFrom

eTo :: IdxEdge v meta -> v
eTo = _eTo

eFromIdx :: IdxEdge v meta -> VertexId
eFromIdx = _eFromIdx

eToIdx :: IdxEdge v meta -> VertexId
eToIdx = _eToIdx
