-- | Transaction of Robert Sedgewick's and Kevin Wayne's IndexMinPQ.java to Haskell.
--
-- https://algs4.cs.princeton.edu/44sp/IndexMinPQ.java.html
module Data.IndexMinPQ where

import Data.Array.ST
import Control.Monad.ST
import qualified Data.Primitive as MV
import qualified Data.Array.MArray as Arr
import Control.Monad (when)

data IndexMinPQ s key = IndexMinPQ
    { state_maxN :: {-# UNPACK #-} !Int
      -- ^ maximum number of elements on PQ
    , state_n :: MV.MutVar s Int
      -- ^ number of elements on PQ
    , state_pq :: STUArray s Int Int
      -- ^ binary heap using 1-based indexing
    , state_qp :: STUArray s Int Int
      -- ^ inverse of pq - qp[pq[i]] = pq[qp[i]] = i
    , state_keys :: STArray s Int (Maybe key)
      -- ^ keys[i] = priority of i
    }

newIndexMinPQ :: Int -> ST s (IndexMinPQ s key)
newIndexMinPQ maxN = do
  -- Translation of IndexMinPQ constructor
  when (maxN < 0) $
    fail $ "newIndexMinPQ: Invalid maxN: " <> show maxN
  n <- MV.newMutVar 0
  pq <- Arr.newArray (0, maxN + 1) 0
  qp <- Arr.newArray (0, maxN + 1) (-1)
  keys <- Arr.newArray (0, maxN + 1) Nothing
  pure $ IndexMinPQ
    { state_maxN = maxN
    , state_n = n
    , state_pq = pq
    , state_qp = qp
    , state_keys = keys
    }

isEmpty
  :: IndexMinPQ s key
  -> ST s Bool
isEmpty pq =
  (== 0) <$> MV.readMutVar (state_n pq)

insert
  :: IndexMinPQ s key
  -> Int
  -> key
  -> ST s ()
insert pq i key = undefined
-- public void insert(int i, Key key) {
--     validateIndex(i);
--     if (contains(i)) throw new IllegalArgumentException("index is already in the priority queue");
--     n++;
--     qp[i] = n;
--     pq[n] = i;
--     keys[i] = key;
--     swim(n);
-- }

delMin
  :: IndexMinPQ s key
  -> ST s Int
delMin pq = undefined
-- public int delMin() {
--     if (n == 0) throw new NoSuchElementException("Priority queue underflow");
--     int min = pq[1];
--     exch(1, n--);
--     sink(1);
--     assert min == pq[n+1];
--     qp[min] = -1;        // delete
--     keys[min] = null;    // to help with garbage collection
--     pq[n+1] = -1;        // not needed
--     return min;
-- }

contains
  :: IndexMinPQ s key
  -> Int
  -> ST s Bool
contains pq i = undefined
-- public boolean contains(int i) {
--     validateIndex(i);
--     return qp[i] != -1;
-- }

decreaseKey
  :: IndexMinPQ s key
  -> Int
  -> key
  -> ST s ()
decreaseKey pq i key = undefined
-- public void decreaseKey(int i, Key key) {
--     validateIndex(i);
--     if (!contains(i)) throw new NoSuchElementException("index is not in the priority queue");
--     if (keys[i].compareTo(key) == 0)
--         throw new IllegalArgumentException("Calling decreaseKey() with a key equal to the key in the priority queue");
--     if (keys[i].compareTo(key) < 0)
--         throw new IllegalArgumentException("Calling decreaseKey() with a key strictly greater than the key in the priority queue");
--     keys[i] = key;
--     swim(qp[i]);
-- }

-- ***************************************************************************
-- * General helper functions.
-- ***************************************************************************

exch
  :: IndexMinPQ s key
  -> Int
  -> Int
  -> ST s ()
exch pq i j = undefined
-- private void exch(int i, int j) {
--     int swap = pq[i];
--     pq[i] = pq[j];
--     pq[j] = swap;
--     qp[pq[i]] = i;
--     qp[pq[j]] = j;
-- }

--  ***************************************************************************
--  * Heap helper functions.
--  ***************************************************************************

swim
  :: IndexMinPQ s key
  -> Int
  -> ST s ()
swim pq k = undefined
-- private void swim(int k) {
--     while (k > 1 && greater(k/2, k)) {
--         exch(k, k/2);
--         k = k/2;
--     }
-- }

sink
  :: IndexMinPQ s key
  -> Int
  -> ST s ()
sink pq k = undefined
-- private void sink(int k) {
--     while (2*k <= n) {
--         int j = 2*k;
--         if (j < n && greater(j, j+1)) j++;
--         if (!greater(k, j)) break;
--         exch(k, j);
--         k = j;
--     }
-- }
