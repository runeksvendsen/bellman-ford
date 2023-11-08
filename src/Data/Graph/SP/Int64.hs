-- | TODO
module Data.Graph.SP.Int64
( addition
, infinity
)
where
import Data.Int (Int64)

infinity :: Int64
infinity = maxBound `div` 2

-- | TODO: docs
addition :: Int64 -> Int64 -> Int64
addition a b
    | a == infinity || b == infinity = infinity
    | otherwise = a + b
