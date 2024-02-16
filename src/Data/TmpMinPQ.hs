module Data.TmpMinPQ
( TmpMinPQ
, new
, push
, pop
, empty
)
where

import qualified Data.IntPSQ as Q
import qualified Data.STRef as Ref
import Control.Monad.ST (ST)
import Control.Monad (forM_)
import Data.Functor ((<&>))

data TmpMinPQ s p v = TmpMinPQ
    { tmpMinPQ_indexCounter :: !(Ref.STRef s Int)
    , tmpMinPQ_queue :: !(Ref.STRef s (Q.IntPSQ p v))
    }

new :: ST s (TmpMinPQ s p v)
new =
    TmpMinPQ <$> Ref.newSTRef 0 <*> Ref.newSTRef Q.empty

-- | Remove all items
empty
    :: TmpMinPQ s p v
    -> ST s ()
empty pq = do
    Ref.writeSTRef (tmpMinPQ_indexCounter pq) 0
    Ref.writeSTRef (tmpMinPQ_queue pq) Q.empty

push
    :: Ord p
    => TmpMinPQ s p v
    -> p
    -> v
    -> ST s ()
push pq p v = do
    size <- Ref.readSTRef $ tmpMinPQ_indexCounter pq
    Ref.modifySTRef' (tmpMinPQ_queue pq) $ Q.insert size p v
    Ref.modifySTRef' (tmpMinPQ_indexCounter pq) (+1)

pop
    :: Ord p
    => TmpMinPQ s p v
    -> ST s (Maybe (p, v))
pop pq = do
    mMin <- Q.findMin <$> Ref.readSTRef (tmpMinPQ_queue pq)
    forM_ mMin $ \(k, _, _) -> do
        Ref.modifySTRef' (tmpMinPQ_queue pq) $ Q.delete k
    pure $ mMin <&> \(_, p, v) -> (p, v)
