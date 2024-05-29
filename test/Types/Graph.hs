{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

-- | Generate an arbitrary graph
module Types.Graph
( arbitraryConnectedGraph, ConnectedGraph(..)
, arbitraryGraphEdges, GraphEdges(..)
, PrettyShow(..)
)
where

import Types.Edge
import Control.Monad (forM, foldM)
import qualified Test.Tasty.QuickCheck as QC
import Data.Maybe (listToMaybe, maybeToList, catMaybes)
import qualified Data.Set as Set
import GHC.TypeLits (KnownNat, Nat, natVal)
import Data.Proxy (Proxy(Proxy))
import qualified Data.List

-- | Generate a list of edges (that hopefully forms a graph)
arbitraryGraphEdges
  :: QC.Arbitrary weight
  => QC.Gen [TestEdge weight]
arbitraryGraphEdges = do
  QC.NonEmpty nodesList <- QC.arbitrary
  weights :: [weight] <- QC.arbitrary
  let nodesListStr = map (show @Int . QC.getPositive) nodesList
  forM weights $ \weight ->
    TestEdge
      <$> QC.elements nodesListStr
      <*> QC.elements nodesListStr
      <*> pure weight

newtype GraphEdges weight = GraphEdges { unGraphEdges :: [TestEdge weight] }
  deriving (Show)

instance QC.Arbitrary weight => QC.Arbitrary (GraphEdges weight) where
  arbitrary = GraphEdges <$> arbitraryGraphEdges
  shrink = map GraphEdges . dropOnes . unGraphEdges

-- | Generate a connected graph of a minimum size
arbitraryConnectedGraph
  :: (QC.Arbitrary weight)
  => Int -- ^ Minimum number of nodes in the graph
  -> QC.Gen [TestEdge weight]
arbitraryConnectedGraph minCount = do
  nodesList <- nonEmptyListMinCount
  let nodesListStr = map (show @Int . QC.getPositive) nodesList
  fst <$> foldM folder ([], Set.fromList $ maybeToList $ listToMaybe nodesListStr) nodesListStr
  where
    nonEmptyListMinCount :: QC.Arbitrary b => QC.Gen [b]
    nonEmptyListMinCount = do
      minCountLengthList <- QC.vector minCount
      restOfList <- QC.arbitrary
      pure $ minCountLengthList ++ restOfList

    folder (edges, edgesNodes) node = do
      isToEdge <- QC.arbitrary
      weight <- QC.arbitrary
      let mkEdge = if isToEdge then TestEdge else flip TestEdge
      existingNode <- QC.elements $ Set.toList edgesNodes
      existingNodeOrNewNode <- QC.elements [[node], Set.toList edgesNodes] >>= QC.elements
      let newEdge = mkEdge existingNode existingNodeOrNewNode weight
          newEdgeNodes = Set.insert existingNodeOrNewNode edgesNodes
      pure (newEdge : edges, newEdgeNodes)

newtype ConnectedGraph (minSize :: Nat) weight = ConnectedGraph { unConnectedGraph :: [TestEdge weight] }
  deriving (Show)

instance (KnownNat minSize, QC.Arbitrary weight) => QC.Arbitrary (ConnectedGraph minSize weight) where
  arbitrary =
    let minSize = fromIntegral $ natVal (Proxy :: Proxy minSize)
    in ConnectedGraph <$> arbitraryConnectedGraph minSize
  shrink = map ConnectedGraph . dropOnes . unConnectedGraph

-- | Return all permutations of the input list with a single element dropped.
--
--  Example:
--
--  >>> dropElement [1,2,3,4,5]
--  [[2,3,4,5],[1,3,4,5],[1,2,4,5],[1,2,3,5],[1,2,3,4]]
dropOnes :: [a] -> [[a]]
dropOnes lst =
  catMaybes $ zipWith dropFirstTail (Data.List.inits lst) (Data.List.tails lst)
  where
    dropFirstTail _ [] = Nothing
    dropFirstTail init' tail' = Just $ init' ++ drop 1 tail'

showGraphEdges
  :: Show weight
  => [TestEdge weight]
  -> String
showGraphEdges edges =
  "[" <> Data.List.intercalate ", " (map prettyShowTestEdge edges) <> "]"

prettyShowTestEdge :: Show a => TestEdge a -> String
prettyShowTestEdge e = getFrom e <> " -> " <> getTo e <> " @ " <> show (getWeight e)

newtype PrettyShow a = PrettyShow { unPrettyShow :: a }
  deriving (Eq)

instance Show weight => Show (PrettyShow (TestEdge weight)) where
  show = prettyShowTestEdge . unPrettyShow

instance Show weight => Show (PrettyShow ([TestEdge weight], weight)) where
  show (PrettyShow (edges, weight)) = "(" <> showGraphEdges edges <> ", " <> show weight <> ")"