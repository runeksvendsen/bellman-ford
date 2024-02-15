module Data.MinPQ
( MinPQ
, new
, push
, pop
, empty
)
where

import qualified Data.IndexMinPQ as Q
import qualified Data.STRef as Ref
import Control.Monad.ST (ST)
import Data.Functor (void)

data MinPQ s item = MinPQ
    { tmpMinPQ_indexCounter :: !(Ref.STRef s Int)
    , tmpMinPQ_queue :: !(Q.IndexMinPQ s item)
    }

new :: Int -> ST s (MinPQ s item)
new initialSize =
    MinPQ <$> Ref.newSTRef 0 <*> Q.newIndexMinPQ initialSize

-- | Remove all items
empty
    :: Ord item
    => MinPQ s item
    -> ST s ()
empty pq = do
    Ref.writeSTRef (tmpMinPQ_indexCounter pq) 0
    void $ Q.emptyAsSortedList (tmpMinPQ_queue pq)

push
    :: Ord item
    => MinPQ s item
    -> item
    -> ST s ()
push pq item = do
    availableIndex <- Ref.readSTRef $ tmpMinPQ_indexCounter pq
    Q.insert (tmpMinPQ_queue pq) availableIndex item
    Ref.modifySTRef' (tmpMinPQ_indexCounter pq) (+1)

pop
    :: Ord item
    => MinPQ s item
    -> ST s (Maybe item)
pop pq = do
    isEmpty <- Q.isEmpty (tmpMinPQ_queue pq)
    if isEmpty
        then pure Nothing
        else do
            (_, item) <- Q.delMinKey (tmpMinPQ_queue pq)
            pure $ Just item
