{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Types.Cycle
( EdgeCycle(..)
, NegativeCycle(..)
)
where

import           Types.Edge
import qualified Test.SmallCheck.Series               as SS
import qualified Data.List.NonEmpty                   as NE
import           Data.Graph.Cycle                     (verifyCycle)
import           Data.Maybe                           (isNothing)
import qualified Control.Exception                    as Ex
import           Control.Applicative                  (empty)
import           Data.List                            (sort, nub)


newtype EdgeCycle = EdgeCycle { getEdgeCycle :: NE.NonEmpty TestEdge }
   deriving (Eq, Show, Ord)

instance Monad m => SS.Serial m EdgeCycle where
   series = do
      edgeList <- edgeCycle
      let isValidCycle = isNothing . verifyCycle . NE.toList
      return $ EdgeCycle $ Ex.assert (isValidCycle edgeList) edgeList

edgeCycle :: Monad m => SS.Series m (NE.NonEmpty TestEdge)
edgeCycle = do
    -- Data.Graph.Cycle does not support self-loops, so we
    --  make sure to generate at least 2 vertices
    vertexList <- uniqueVertices `suchThat` ((> 1) . length)
    --    Example data transformation for (pairUp . headToLast . duplicate):
    --       [a, b, c, d]
    --    => [a, a, b, b, c, c, d, d]
    --    => [a, b, b, c, c, d, d, a]
    --    => [(a,b), (b,c), (c,d), (d,a)]
    let vertexPairs = pairUp $ headToLast (duplicate $ NE.toList vertexList)
        toEdgeM (from,to) = TestEdge from to <$> SS.series
    NE.fromList <$> mapM toEdgeM vertexPairs
  where
    headToLast :: [a] -> [a]
    headToLast [] = []
    headToLast (x:xs) = xs ++ [x]
    pairUp :: Show a => [a] -> [(a,a)]
    pairUp [] = []
    pairUp lst@[_] = error $ "odd-lengthed list: " ++ show lst
    pairUp (x:y:xs) = (x,y):pairUp xs
    duplicate [] = []
    duplicate (x:xs) = x:x:duplicate xs

uniqueVertices :: Monad m => SS.Series m (NE.NonEmpty String)
uniqueVertices =
   NE.fromList <$> uniqueElemList `suchThat` ((> 0) . length)

uniqueElemList :: (Ord a, SS.Serial m a) => SS.Series m [a]
uniqueElemList = 
    SS.series `suchThat` containsUniqueElements
  where
   containsUniqueElements list = 
      (length . nub . sort $ list) == length list

suchThat :: SS.Series m a -> (a -> Bool) -> SS.Series m a
suchThat s p = s >>= \x -> if p x then pure x else empty

newtype NegativeCycle = NegativeCycle { getNegativeCycle :: NE.NonEmpty TestEdge }
   deriving (Eq, Show, Ord)

instance Monad m => SS.Serial m NegativeCycle where
   series = do
      let negativeWeightSum edgeCycle' =
            sum (NE.map getWeight $ getEdgeCycle edgeCycle') < 0
      NegativeCycle . getEdgeCycle <$> SS.series `suchThat` negativeWeightSum
