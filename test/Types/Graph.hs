{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Generate an arbitrary graph
module Types.Graph
( arbitraryConnectedGraph, arbitraryGraphEdges
)
where

import Types.Edge
import Control.Monad (forM, foldM)
import qualified Test.Tasty.QuickCheck as QC
import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.Set as Set

-- | Generate a list of edges (that hopefully forms a graph)
arbitraryGraphEdges
  :: QC.Arbitrary a
  => (a -> weight)
  -> QC.Gen [TestEdge weight]
arbitraryGraphEdges graphModifier = do
  QC.NonEmpty nodesList <- QC.arbitrary
  weights :: [weight] <- QC.arbitrary
  let nodesListStr = map (show @Int . QC.getPositive) nodesList
  forM weights $ \weight ->
    TestEdge
      <$> QC.elements nodesListStr
      <*> QC.elements nodesListStr
      <*> pure (graphModifier weight)

-- | Generate a connected graph of a minimum size
arbitraryConnectedGraph
  :: forall a weight.
     (QC.Arbitrary a)
  => (a -> weight)
  -> Int -- ^ Minimum number of nodes in the graph
  -> QC.Gen [TestEdge weight]
arbitraryConnectedGraph graphModifier minCount = do
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
      let weight' = graphModifier weight
          mkEdge = if isToEdge then TestEdge else flip TestEdge
      existingNode <- QC.elements $ Set.toList edgesNodes
      existingNodeOrNewNode <- QC.elements [[node], Set.toList edgesNodes] >>= QC.elements
      let newEdge = mkEdge existingNode existingNodeOrNewNode weight'
          newEdgeNodes = Set.insert existingNodeOrNewNode edgesNodes
      pure (newEdge : edges, newEdgeNodes)
