module Queue.Spec
( spec )
where

import           Data.Graph.Prelude
import qualified Util.QuickSmall                    as QS
import qualified Data.Queue                         as Lib

import qualified Control.Monad.ST                   as ST
import qualified Test.Hspec.SmallCheck              ()
import           Test.Hspec.Expectations.Pretty            (Expectation, shouldBe)
import qualified Test.Tasty                         as Tasty


spec :: Tasty.TestTree
spec = Tasty.testGroup "Queue" $
    [ Tasty.testGroup "dequeue"
        [ QS.testProperty "returns enqueued items" enqueueListDeque
        ]
    , Tasty.testGroup "toList"
        [ QS.testProperty "yields enqueued items" enqueueListStream
        ]
    ]

enqueueListDeque
    :: [Int]
    -> Expectation
enqueueListDeque items =  do
    dequeuedList <- ST.stToIO $ do
            queue <- Lib.new
            forM_ items (Lib.enqueue queue)
            dequeueToList [] queue
    dequeuedList `shouldBe` reverse items
  where
    dequeueToList accum q = do
        itemM <- Lib.dequeue q
        case itemM of
            Nothing   -> return accum
            Just item -> dequeueToList (item : accum) q

enqueueListStream
    :: [Int]
    -> Expectation
enqueueListStream items =  do
    dequeuedList <- ST.stToIO $ do
            queue <- Lib.new
            forM_ items (Lib.enqueue queue)
            Lib.toList queue
    dequeuedList `shouldBe` reverse items
