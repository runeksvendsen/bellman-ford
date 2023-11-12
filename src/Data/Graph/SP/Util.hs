module Data.Graph.SP.Util
( showIndexedVertex
, showEdge
)
where
import qualified Data.Graph.Digraph as DG

showIndexedVertex
    :: Show v
    => (v, DG.VertexId)
    -> String
showIndexedVertex (v, vid) = show (DG.vidInt vid) <> " (" <> show v <> ")"

showEdge
    :: (Show meta, Show v)
    => DG.IdxEdge v meta
    -> String
showEdge e = unwords
    [ showIndexedVertex (DG.eFrom e, DG.eFromIdx e)
    , "->"
    , showIndexedVertex (DG.eTo e, DG.eToIdx e)
    , "(meta:"
    , show (DG.eMeta e) <> ")"
    ]
