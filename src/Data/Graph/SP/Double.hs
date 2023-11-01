-- | Shortest-path logic related to using 'Double' as edge weights.
--
--  This must account for rounding errors, cf. https://stackoverflow.com/a/65051801/700597.
module Data.Graph.SP.Double
( isLessThan
)
where

-- | Use this as the @isLessThan@ function when using 'Double' as edge weight.
--
--   Necessary because of floating point rounding errors.
--   Cf. https://stackoverflow.com/a/65051801/700597
isLessThan :: Double -> Double -> Bool
isLessThan a b =
    a + epsilon < b
    where
        epsilon :: Double
        epsilon = 1.0e-14
