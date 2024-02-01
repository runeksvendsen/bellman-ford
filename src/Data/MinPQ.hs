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

data MinPQ s p v = MinPQ
    { tmpMinPQ_indexCounter :: !(Ref.STRef s Int)
    , tmpMinPQ_queue :: !(Q.IndexMinPQ s (QueueItem p v))
    }

data QueueItem p v = QueueItem !p !v

instance Eq p => Eq (QueueItem p v) where
    QueueItem p1 _ == QueueItem p2 _ = p1 == p2

instance Ord p => Ord (QueueItem p v) where
    QueueItem p1 _ <= QueueItem p2 _ = p1 <= p2

new :: Int -> ST s (MinPQ s p v)
new initialSize =
    MinPQ <$> Ref.newSTRef 0 <*> Q.newIndexMinPQ initialSize

-- | Remove all items
empty
    :: (Ord p)
    => MinPQ s p v
    -> ST s ()
empty pq = do
    Ref.writeSTRef (tmpMinPQ_indexCounter pq) 0
    void $ Q.emptyAsSortedList (tmpMinPQ_queue pq)

push
    :: (Ord p)
    => MinPQ s p v
    -> p
    -> v
    -> ST s ()
push pq p v = do
    availableIndex <- Ref.readSTRef $ tmpMinPQ_indexCounter pq
    Q.insert (tmpMinPQ_queue pq) availableIndex (QueueItem p v)
    Ref.modifySTRef' (tmpMinPQ_indexCounter pq) (+1)

pop
    :: (Ord p)
    => MinPQ s p v
    -> ST s (Maybe (p, v))
pop pq = do
    isEmpty <- Q.isEmpty (tmpMinPQ_queue pq)
    if isEmpty
        then pure Nothing
        else do
            (_, QueueItem p v) <- Q.delMinKey (tmpMinPQ_queue pq)
            pure $ Just (p, v)
