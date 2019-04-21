{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Types.Cycle
( EdgeCycle(..)
, NegativeCycle(..)
)
where

import           Types.Edge
import           Util.GenData                         (GenData(..), SuchThat(..))
import qualified Test.SmallCheck.Series               as SS
import qualified Test.Tasty.QuickCheck                as QC
import qualified Data.List.NonEmpty                   as NE
import           Data.Graph.Cycle                     (verifyCycle)
import           Data.Maybe                           (isNothing)
import qualified Control.Exception                    as Ex
import           Data.List                            (sort, nub)


newtype EdgeCycle = EdgeCycle { getEdgeCycle :: NE.NonEmpty TestEdge }
   deriving (Eq, Show, Ord)

instance Monad m => SS.Serial m EdgeCycle where
   series = edgeCycle

instance QC.Arbitrary EdgeCycle where
   arbitrary = edgeCycle

edgeCycle
   :: ( GenData m [String]
      , GenData m Double
      )
   => m EdgeCycle
edgeCycle = do
   edgeList <- edgeCycleEdges
   let isValidCycle = isNothing . verifyCycle . NE.toList
   return $ EdgeCycle $ Ex.assert (isValidCycle edgeList) edgeList

edgeCycleEdges
   :: ( GenData m [String]
      , GenData m Double
      )
   => m (NE.NonEmpty TestEdge)
edgeCycleEdges = do
    -- Data.Graph.Cycle does not support self-loops, so we
    --  make sure to generate at least 2 vertices
    vertexList <- uniqueElemList `suchThat` ((> 1) . length)
    --    Example data transformation for (pairUp . headToLast . duplicate):
    --       [a, b, c, d]
    --    => [a, a, b, b, c, c, d, d]
    --    => [a, b, b, c, c, d, d, a]
    --    => [(a,b), (b,c), (c,d), (d,a)]
    let vertexPairs = pairUp $ headToLast (duplicate vertexList)
        toEdgeM (from,to) = TestEdge from to <$> genData
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

uniqueElemList :: (Ord a, GenData m [a]) => m [a]
uniqueElemList =
    genData `suchThat` containsUniqueElements
  where
   containsUniqueElements list =
      (length . nub . sort $ list) == length list

newtype NegativeCycle = NegativeCycle { getNegativeCycle :: NE.NonEmpty TestEdge }
   deriving (Eq, Show, Ord)

instance Monad m => SS.Serial m NegativeCycle where
   series = negativeCycle

instance QC.Arbitrary NegativeCycle where
   arbitrary = negativeCycle

negativeCycle
   :: ( GenData m EdgeCycle
      )
   => m NegativeCycle
negativeCycle =
    NegativeCycle . getEdgeCycle <$> genData `suchThat` negativeWeightSum
  where
    negativeWeightSum edgeCycle' =
        sum (NE.map getWeight $ getEdgeCycle edgeCycle') < 0
