module Data.Graph.Orphans
(
)
where

import           Data.Graph.Types.Internal          (Vertex(Vertex, getVertexInternal))
import           Data.Ix                            (Ix(..))


instance Ix (Vertex g) where
    range (start, end) =
        fmap Vertex $ range (vtxInt start, vtxInt end)
    index (start, end) sub =
        index (vtxInt start, vtxInt end) (vtxInt sub)
    inRange (start, end) sub =
        inRange (vtxInt start, vtxInt end) (vtxInt sub)

vtxInt = getVertexInternal
