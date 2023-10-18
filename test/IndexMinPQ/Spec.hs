{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module IndexMinPQ.Spec
( spec )
where

import Data.Graph.Prelude
import IndexMinPQ.Util
import qualified Util.QuickSmall as QS
import qualified Data.IndexMinPQ as Lib
import qualified Control.Monad.ST as ST
import Test.Hspec.Expectations (Expectation, shouldBe)
import qualified Test.Tasty as Tasty
import qualified Test.Hspec.SmallCheck () -- Apparently has an instance for "Test.QuickCheck.Property.Testable (IO ())"
import qualified System.Random.Shuffle as Shuffle
import Data.List (sortOn, groupBy)

spec :: Tasty.TestTree
spec = Tasty.testGroup "IndexMinPQ" $
    [ Tasty.testGroup "'asSortedList' on queue with items inserted in shuffled order"
        [ QS.testProperty "returns sorted items" $ \priorities ->
            enqueueListDeque (map (,Nothing) priorities)
        , QS.testProperty "with arbitrary 'decreaseKey': returns sorted items" enqueueListDeque
        ]
    ]

type Priority = Int

enqueueListDeque
    :: [(Priority, Maybe (Positive Priority))] -- ^ (Initial priority, maybe amount to decrease priority)
    -> Expectation
enqueueListDeque priorities' = do
    shuffledIndexedItems <- Shuffle.shuffleM indexedItems
    let shuffledIndexedItemsWithoutAdjustments = map (fmap fst) shuffledIndexedItems
    shuffledIndexedItems2 <- Shuffle.shuffleM shuffledIndexedItems
    shuffledAdjustedPrioIndexedItems <- Shuffle.shuffleM adjustedPrioIndexedItems
    dequeuedList <- ST.stToIO $ do
        queue <- Lib.newIndexMinPQ (length priorities)
        forM_ shuffledIndexedItemsWithoutAdjustments $ \(i, item) ->
            Lib.insert queue i item
        forM_ shuffledIndexedItems2 $ \(index, (originalPrio, mDecreasePrio)) ->
            forM_ mDecreasePrio $ \decreasePrio ->
                Lib.decreaseKey queue index (originalPrio - decreasePrio)
        Lib.asSortedList queue
    let sortEqualPrioritySublists :: [(Int, Priority)] -> [(Int, Priority)]
        sortEqualPrioritySublists lst =
            -- NOTE: If we only sort by 'Priority', then equal priorities have an undefined order.
            --       For example, if we put the list [(0, 1), (1, 1)] into the queue then 'Lib.asSortedList' may return it in either order.
            --       To solve this, we split the list into sublists of adjacent equal priority, and sort
            --       these sublists by the index (which is unique), thereby making the order of adjacent
            --       equal priorities well-defined (based on the index).
            --       Another option would be to use unique priorities, but this gets overly complicated when
            --       testing 'Lib.decreaseKey' (because then the resulting adjusted priority must not be equal to any other priority).
            concat $ map (sortOn fst) (groupOn snd lst)
    sortEqualPrioritySublists dequeuedList
        `shouldBe`
            sortEqualPrioritySublists (sortOn snd shuffledAdjustedPrioIndexedItems)
    where
        priorities = map (fmap $ fmap unPositive) priorities'

        adjustedPrioIndexedItems = map
            (\(index, (originalPrio, mDecreasePrio)) ->
                let decreasePrio = fromMaybe 0 mDecreasePrio
                in (index, originalPrio - decreasePrio)
            )
            indexedItems

        indexedItems :: [(Int, (Priority, Maybe Priority))]
        indexedItems = zip [0..] priorities

        groupOn f = groupBy (\a1 a2 -> f a1 == f a2)
