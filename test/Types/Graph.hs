{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Generate an arbitrary graph
module Types.Graph
( arbitraryGraph
, intToStr -- TODO: not used
)
where

import Types.Edge
import Control.Monad (forM)
import qualified Test.Tasty.QuickCheck as QC


arbitraryGraph
  :: QC.Arbitrary a => (a -> weight) -> QC.Gen [TestEdge weight]
arbitraryGraph graphModifier = do
  QC.NonEmpty nodesList <- QC.arbitrary
  weights :: [weight] <- QC.arbitrary
  let nodesListStr = map (show @Int . QC.getPositive) nodesList
  forM weights $ \weight ->
    TestEdge
      <$> QC.elements nodesListStr
      <*> QC.elements nodesListStr
      <*> pure (graphModifier weight)

-- map an Int to a sequence of characters A-Z
intToStr :: Int -> String
intToStr int =
  go [] int
  where
    go accum i =
      let (div', mod') = i `divMod` charCount
          newChar = toEnum $ mod' + baseInt
          newAccum = newChar : accum
      in if div' == 0
        then newAccum
        else go newAccum div'

    charCount = fromEnum 'Z' - fromEnum 'A' + 1
    baseInt = fromEnum 'A'
