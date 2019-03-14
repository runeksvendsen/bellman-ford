module Queue.Spec
( spec )
where

import           Data.Graph.Prelude
import qualified Data.Queue                         as Lib

import qualified Data.Vector.Fusion.Stream.Monadic  as S
import qualified Control.Monad.ST                   as ST
import qualified Test.Hspec.SmallCheck              as SC
import           Test.Hspec                         (Spec, Expectation, shouldBe)
import           Test.Hspec                         (describe, it, parallel)


spec :: Spec
spec = parallel $ do
    describe "dequeue" $ do
        it "returns enqueued items" $
            SC.property enqueueListDeque
    describe "mstream" $ do
        it "yields enqueued items" $
            SC.property enqueueListStream

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
            S.toList =<< Lib.mstream queue
    dequeuedList `shouldBe` items
