{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Generate an arbitrary graph
module Types.Graph
( arbitraryGraph, arbitraryGraphOld
)
where

import Types.Edge
import Control.Monad (forM, foldM)
import qualified Test.Tasty.QuickCheck as QC


arbitraryGraphOld
  :: QC.Arbitrary a
  => (a -> weight)
  -> QC.Gen [TestEdge weight]
arbitraryGraphOld graphModifier = do
  QC.NonEmpty nodesList <- QC.arbitrary
  weights :: [weight] <- QC.arbitrary
  let nodesListStr = map (show @Int . QC.getPositive) nodesList
  forM weights $ \weight ->
    TestEdge
      <$> QC.elements nodesListStr
      <*> QC.elements nodesListStr
      <*> pure (graphModifier weight)

-- | Generate a connected graph of a minimum size
arbitraryGraph
  :: forall a weight.
     (QC.Arbitrary a)
  => (a -> weight)
  -> Int -- ^ Minimum number of nodes in the graph
  -> QC.Gen [TestEdge weight]
arbitraryGraph graphModifier minCount = do
  nodesList <- nonEmptyListMinCount
  let nodesListStr = map (show @Int . QC.getPositive) nodesList
  fst <$> foldM folder ([], [head nodesListStr]) nodesListStr
  where
    nonEmptyListMinCount :: QC.Arbitrary b => QC.Gen [b]
    nonEmptyListMinCount = QC.getNonEmpty <$>
      QC.arbitrary `QC.suchThat` \(QC.NonEmpty lst) ->
        length lst >= minCount

    folder (edges, edgesNodes) node = do
      isToEdge <- QC.arbitrary
      weight <- QC.arbitrary
      let weight' = graphModifier weight
          mkEdge from to = if isToEdge then TestEdge from to weight' else TestEdge to from weight'
      otherEdgeNode <- QC.elements edgesNodes
      let newEdge = mkEdge node otherEdgeNode
      pure (newEdge : edges, node : edgesNodes)
