module IndexMinPQ.Spec
( spec )
where

import           Data.Graph.Prelude
import qualified Util.QuickSmall                    as QS
import qualified Data.IndexMinPQ                    as Lib

import qualified Control.Monad.ST                   as ST
import qualified Test.Hspec.SmallCheck              ()
import           Test.Hspec.Expectations            (Expectation, shouldBe)
import qualified Test.Tasty                         as Tasty
import qualified System.Random.Shuffle              as Shuffle
import Data.List (sortOn)


spec :: Tasty.TestTree
spec = Tasty.testGroup "IndexMinPQ" $
    [ Tasty.testGroup "asSortedList"
        [ QS.testProperty "returns sorted items" enqueueListDeque
        ]
    ]

enqueueListDeque
    :: [Int] -- ^ TODO: Unique list
    -> Expectation
enqueueListDeque items = do
    shuffledIndexedItems <- Shuffle.shuffleM indexedItems
    dequeuedList <- ST.stToIO $ do
        queue <- Lib.newIndexMinPQ (length items)
        forM_ shuffledIndexedItems $ \(i, item) ->
            Lib.insert queue i item
        Lib.asSortedList queue
    dequeuedList `shouldBe` sortOn snd indexedItems
    where
        indexedItems = zip [0..] items
