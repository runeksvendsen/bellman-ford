{-# LANGUAGE BangPatterns #-}
-- | Transaction of Robert Sedgewick and Kevin Wayne's @IndexMinPQ.java@ to Haskell.
--
-- https://algs4.cs.princeton.edu/44sp/IndexMinPQ.java.html
module Data.IndexMinPQ
( IndexMinPQ
, newIndexMinPQ
, isEmpty
, insert
, delMin
, contains
, decreaseKey
)
where

import Data.Graph.Prelude
import Data.Array.ST
import qualified Data.Primitive as MV
import qualified Data.Array.MArray as Arr

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
  :: Ord key
  => IndexMinPQ s key
  -> Int
  -> key
  -> ST s ()
insert pq i key = do
  whenM (contains pq i) $
    fail $ "index is already in the priority queue: " <> show i
  n <- modifyMutVar (state_n pq) (+ 1) -- n++
  Arr.writeArray (state_qp pq) i n -- qp[i] = n
  Arr.writeArray (state_pq pq) n i -- pq[n] = i
  Arr.writeArray (state_keys pq) i (Just key) -- keys[i] = key
  swim pq n -- swim(n)

delMin
  :: Ord key
  => IndexMinPQ s key
  -> ST s Int
delMin pq = do
  whenM ((== 0) <$> MV.readMutVar (state_n pq)) $
    fail "Priority queue underflow"
  min' <- Arr.readArray (state_pq pq) 1 -- int min = pq[1]
  n <- modifyMutVar (state_n pq) (subtract 1) -- n--
  exch pq 1 n -- exch(1, n)
  sink pq 1 -- sink(1)
  unlessM ((== min') <$> Arr.readArray (state_pq pq) (n+1)) $
    fail "Assertion failed: min == pq[n+1]"  -- assert min == pq[n+1]
  Arr.writeArray (state_qp pq) min' (-1) -- qp[min] = -1
  Arr.writeArray (state_keys pq) min' Nothing -- keys[min] = null
  Arr.writeArray (state_pq pq) (n+1) (-1) -- pq[n+1] = -1
  pure min'

contains
  :: IndexMinPQ s key
  -> Int
  -> ST s Bool
contains q i = do
  (/= -1) <$> Arr.readArray (state_qp q) i -- return qp[i] != -1;

decreaseKey
  :: (Ord key, Show key)
  => IndexMinPQ s key
  -> Int
  -> key
  -> ST s ()
decreaseKey pq i key = do
  whenM (not <$> contains pq i) $ -- if (!contains(i))
    fail $ "index is not in the priority queue: " <> show i
  iKey <- Arr.readArray (state_keys pq) i >>= maybe (fail $ "decreaseKey: no such index: " <> show i) pure
  when (iKey == key) $ -- if (keys[i].compareTo(key) == 0)
    fail $ "Calling decreaseKey() with a key equal to the key in the priority queue: " <> show (i, key)
  when (key > iKey) $ -- if (keys[i].compareTo(key) < 0)
    fail $ "Calling decreaseKey() with a key strictly greater than the key in the priority queue: " <> show (i, key, iKey)
  Arr.writeArray (state_keys pq) i (Just key) -- keys[i] = key
  Arr.readArray (state_qp pq) i >>= swim pq -- swim(qp[i])

-- ***************************************************************************
-- * General helper functions.
-- ***************************************************************************

greater
  :: Ord key
  => IndexMinPQ s key
  -> Int
  -> Int
  -> ST s Bool
greater pq i j = do
  pqI <- Arr.readArray (state_pq pq) i
  pqJ <- Arr.readArray (state_pq pq) j
  pqIKey <- Arr.readArray (state_keys pq) pqI
  pqJKey <- Arr.readArray (state_keys pq) pqJ
  pure $ pqIKey > pqJKey

exch
  :: IndexMinPQ s key
  -> Int
  -> Int
  -> ST s ()
exch pq i j = do
  swap <- Arr.readArray (state_pq pq) i -- int swap = pq[i];
  oldPqJ <- Arr.readArray (state_pq pq) j -- "save the old pq[j] because we need to use it twice"
  Arr.writeArray (state_pq pq) i oldPqJ -- pq[i] = pq[j];
  Arr.writeArray (state_pq pq) j swap -- pq[j] = swap;
  Arr.writeArray (state_qp pq) oldPqJ i -- qp[oldPqJ] = i;
  Arr.writeArray (state_qp pq) swap j -- qp[swap] = j;

--  ***************************************************************************
--  * Heap helper functions.
--  ***************************************************************************

swim
  :: Ord key
  => IndexMinPQ s key
  -> Int
  -> ST s ()
swim pq k =
  when (k > 1) $ do
    let halfOfK = k `div` 2
    whenM (greater pq halfOfK k) $ do
      exch pq k halfOfK -- exch(k, k/2);
      swim pq halfOfK -- k = k/2 (also acts as the "while" by recursing)

sink
  :: Ord key => IndexMinPQ s key
  -> Int
  -> ST s ()
sink pq k = do
  n <- MV.readMutVar (state_n pq)
  when (2*k <= n) $ do
    let j = 2*k -- int j = 2*k
    jIsGreaterThanJPlusOne <- greater pq j (j+1)
    if (j < n && jIsGreaterThanJPlusOne) -- if (j < n && greater(j, j+1))
      then exchAndRecurse (j+1)
      else exchAndRecurse j
  where
    -- does everything after the line
    --   if (j < n && greater(j, j+1)) j++;
    -- with either the original/unincremented "j" or the incremented "j"
    exchAndRecurse j =
      whenM (greater pq k j) $ do -- if (!greater(k, j)) break;
        exch pq k j -- exch(k, j)
        sink pq j -- k = j (also acts as the "while" by recursing)

--  ***************************************************************************
--  * Haskell helper functions.
--  ***************************************************************************

modifyMutVar
  :: PrimMonad m
  => MV.MutVar (PrimState m) a
  -> (a -> a)
  -> m a
modifyMutVar mv f =
  MV.atomicModifyMutVar' mv (\n -> let !n' = f n in (n', n')) -- n++
