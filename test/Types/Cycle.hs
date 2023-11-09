{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Types.Cycle
( EdgeCycle(..)
, NegativeCycle(..)
)
where

import           Types.Edge
import           Util.GenData
import qualified Test.SmallCheck.Series               as SS
import qualified Test.Tasty.QuickCheck                as QC
import qualified Data.List.NonEmpty                   as NE
import           Data.Graph.Cycle                     (verifyCycle)
import           Data.Maybe                           (isNothing)
import qualified Control.Exception                    as Ex


newtype EdgeCycle weight = EdgeCycle { getEdgeCycle :: NE.NonEmpty (TestEdge weight) }
   deriving (Eq, Show, Ord)

instance (Monad m, SS.Serial m weight, Show weight) => SS.Serial m (EdgeCycle weight) where
   series = edgeCycle

instance (QC.Arbitrary weight, Show weight) => QC.Arbitrary (EdgeCycle weight) where
   arbitrary = edgeCycle

edgeCycle
   :: ( GenData m [String]
      , GenData m weight
      , Show weight
      )
   => m (EdgeCycle weight)
edgeCycle = do
   edgeList <- edgeCycleEdges
   let isValidCycle = isNothing . verifyCycle . NE.toList
   return $ EdgeCycle $ Ex.assert (isValidCycle edgeList) edgeList

edgeCycleEdges
   :: ( GenData m weight
      )
   => m (NE.NonEmpty (TestEdge weight))
edgeCycleEdges = do
    -- Data.Graph.Cycle does not support self-loops, so we
    --  make sure to generate at least 2 vertices
    vertexList <- genVertexList
    --    Example data transformation for (pairUp . headToLast . duplicate):
    --       [a, b, c, d]
    --    => [a, a, b, b, c, c, d, d]
    --    => [a, b, b, c, c, d, d, a]
    --    => [(a,b), (b,c), (c,d), (d,a)]
    let vertexPairs = pairUp $ headToLast (duplicate $ NE.toList vertexList)
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

genVertexList :: ArbData m => m (NE.NonEmpty String)
genVertexList = do
   k <- getListSize
   pure $ "A1" NE.:| take k idList
   where
      idList =
         let baseIds = ['A'..'Z']
             go !n = map (replicate n) baseIds : go (n+1)
         in concat (go 1)

newtype NegativeCycle weight = NegativeCycle { getNegativeCycle :: NE.NonEmpty (TestEdge weight) }
   deriving (Eq, Show, Ord)

instance (Monad m, SS.Serial m weight, Show weight, Ord weight, Num weight) => SS.Serial m (NegativeCycle weight) where
   series = negativeCycle

instance (QC.Arbitrary weight, Show weight, Ord weight, Num weight) => QC.Arbitrary (NegativeCycle weight) where
   arbitrary = negativeCycle

negativeCycle
   :: ( GenData m (EdgeCycle weight)
      , Ord weight
      , Num weight
      , Show weight
      )
   => m (NegativeCycle weight)
negativeCycle = fmap assertNegativeWeightSum $
    NegativeCycle . getEdgeCycle <$> genData `suchThat` ((< 0) . negativeWeightSum . getEdgeCycle)
  where
    negativeWeightSum edgeCycle' =
        sum (NE.map getWeight edgeCycle')

    assertNegativeWeightSum c =
      let c' = getNegativeCycle c
      in if negativeWeightSum c' < 0
         then c
         else error $
            "BUG: negativeCycle: generated non-negative cycle: "
               <> show (negativeWeightSum c')
               <> " " <> show c'
