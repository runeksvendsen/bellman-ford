{-# LANGUAGE BangPatterns #-}
-- | Transaction of Robert Sedgewick and Kevin Wayne's @IndexMinPQ.java@ to Haskell.
--
-- https://algs4.cs.princeton.edu/44sp/IndexMinPQ.java.html
module Data.IndexMinPQ
( IndexMinPQ
, newIndexMinPQ
, isEmpty
, insert
, minKey
, delMin
, contains
, decreaseKey
, emptyAsSortedList
)
where

import Data.Graph.Prelude
import Data.Array.ST
import qualified Data.Primitive as MV
import qualified Data.Array.MArray as Arr
import Debug.Trace (traceM)

-- | Indexed min priority queue.
--
-- See https://algs4.cs.princeton.edu/44sp/IndexMinPQ.java.html.
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
  , indexMinPQ_trace :: !Bool
    -- ^ print debug/trace info
  }

assertFail
  :: Show key
  => IndexMinPQ s key
  -> String
  -> ST s a
assertFail pq msg = do
  dbgInfo <- debugShowState pq
  fail $ unwords [msg <> ".", dbgInfo]

debugTrace
  :: Show key
  => IndexMinPQ s key
  -> String
  -> ST s ()
debugTrace pq msg = when (indexMinPQ_trace pq) $ do
  dbgInfo <- debugShowState pq
  traceM $ msg <> ": " <> dbgInfo

debugShowState
  :: Show key
  => IndexMinPQ s key
  -> ST s String
debugShowState pq = do
  n <- MV.readMutVar (state_n pq)
  pq' <- showArray (state_pq pq)
  qp <- showArray (state_qp pq)
  keys <- showArray (state_keys pq)
  pure $ unwords
    [ "IndexMinPQ {"
    , record "maxN" (show $ state_maxN pq)
    , record "n" (show n)
    , record "pq" pq'
    , record "qp" qp
    , record "keys" keys
    , "}"
    ]
  where
    record name val = name <> "=" <> val

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
    { indexMinPQ_trace = False
    , state_maxN = maxN
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
  :: (Ord key, Show key)
  => IndexMinPQ s key
  -> Int
  -> key
  -> ST s ()
insert pq i key = do
  validateIndex pq i
  whenM (contains pq i) $
    fail $ "index is already in the priority queue: " <> show i
  n <- modifyMutVar (state_n pq) (+ 1) -- n++
  Arr.writeArray (state_qp pq) i n -- qp[i] = n
  Arr.writeArray (state_pq pq) n i -- pq[n] = i
  Arr.writeArray (state_keys pq) i (Just key) -- keys[i] = key
  swim pq n -- swim(n)
  debugTrace pq $ "insert " <> show i <> " " <> show key

minIndex
  :: Show key
  => IndexMinPQ s key
  -> ST s Int
minIndex pq = do
  assertQueueNotEmpty pq
  Arr.readArray (state_pq pq) 1 -- return pq[1]

minKey
  :: Show key
  => IndexMinPQ s key
  -> ST s key
minKey pq = do
  minKeyIndex <- minIndex pq -- pq[1]
  mKey <- Arr.readArray (state_keys pq) minKeyIndex -- keys[pq[1]]
  maybe (failOnBug minKeyIndex) pure mKey
  where
    failOnBug minKeyIndex = do
      assertFail pq $ "BUG: minKey: no key for index: " <> show minKeyIndex

delMin
  :: (Ord key, Show key)
  => IndexMinPQ s key
  -> ST s Int
delMin pq = do
  assertQueueNotEmpty pq
  min' <- Arr.readArray (state_pq pq) 1 -- int min = pq[1]
  prevN <- MV.atomicModifyMutVar' (state_n pq) (\n -> (n-1, n)) -- decrement "n", return original "n"
  exch pq 1 prevN -- exch(1, n--) -- NOTE: the unincremented "n" is passed to "exch" since the postfix decrement operator is used
  sink pq 1 -- sink(1)
  unlessM ((== min') <$> Arr.readArray (state_pq pq) prevN) $
    assertFail pq "Assertion failed: min == pq[n+1]"  -- assert min == pq[n+1]
  Arr.writeArray (state_qp pq) min' (-1) -- qp[min] = -1
  Arr.writeArray (state_keys pq) min' Nothing -- keys[min] = null
  Arr.writeArray (state_pq pq) prevN (-1) -- pq[n+1] = -1
  debugTrace pq "delMin"
  pure min'

contains
  :: IndexMinPQ s key
  -> Int
  -> ST s Bool
contains pq i = do
  validateIndex pq i
  (/= -1) <$> Arr.readArray (state_qp pq) i -- return qp[i] != -1;

decreaseKey
  :: (Ord key, Show key)
  => IndexMinPQ s key
  -> Int
  -> key
  -> ST s ()
decreaseKey pq i key = do
  validateIndex pq i
  whenM (not <$> contains pq i) $ -- if (!contains(i))
    fail $ "index is not in the priority queue: " <> show i
  iKey <- Arr.readArray (state_keys pq) i >>= maybe (fail $ "decreaseKey: no such index: " <> show i) pure
  when (iKey == key) $ -- if (keys[i].compareTo(key) == 0)
    fail $ "Calling decreaseKey() with a key equal to the key in the priority queue: " <> show (i, key)
  when (key > iKey) $ -- if (keys[i].compareTo(key) < 0)
    fail $ "Calling decreaseKey() with a key strictly greater than the key in the priority queue: " <> show (i, key, iKey)
  Arr.writeArray (state_keys pq) i (Just key) -- keys[i] = key
  Arr.readArray (state_qp pq) i >>= swim pq -- swim(qp[i])
  debugTrace pq $ "decreaseKey " <> show i <> " " <> show key

-- | Empty the queue and return the elements as a sorted list (increasing order)
emptyAsSortedList
  :: (Ord key, Show key)
  => IndexMinPQ s key
  -> ST s [(Int, key)]
emptyAsSortedList pq =
  go
  where
    go = do
      n <- MV.readMutVar (state_n pq)
      if n == 0
        then pure []
        else do
          key <- minKey pq
          i <- delMin pq
          ((i, key) :) <$> go

-- ***************************************************************************
-- * General helper functions.
-- ***************************************************************************

validateIndex
  :: IndexMinPQ s key
  -> Int
  -> ST s ()
validateIndex pq i = do
  when (i < 0) $
    fail $ "index is negative: " ++ show i
  when (i >= state_maxN pq) $
    fail $ "index >= capacity: " ++ show i

assertQueueNotEmpty
  :: IndexMinPQ s key
  -> ST s ()
assertQueueNotEmpty pq =
  whenM ((== 0) <$> MV.readMutVar (state_n pq)) $
    fail "Priority queue underflow"

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
  :: Show key
  => IndexMinPQ s key
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
  debugTrace pq $ "exch " <> show i <> " " <> show j

--  ***************************************************************************
--  * Heap helper functions.
--  ***************************************************************************

swim
  :: (Ord key, Show key)
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
  :: (Ord key, Show key)
  => IndexMinPQ s key
  -> Int
  -> ST s ()
sink pq k' = do
  go k'
  debugTrace pq $ "sink " <> show k'
  where
    go k = do
      n <- MV.readMutVar (state_n pq)
      when (2*k <= n) $ do
        let j = 2*k -- int j = 2*k
        jIsGreaterThanJPlusOne <- greater pq j (j+1)
        if (j < n && jIsGreaterThanJPlusOne) -- if (j < n && greater(j, j+1))
          then exchAndRecurse k (j+1)
          else exchAndRecurse k j

    -- does everything after the line
    --   if (j < n && greater(j, j+1)) j++;
    -- with either the original/unincremented "j" or the incremented "j"
    exchAndRecurse k j =
      whenM (greater pq k j) $ do -- if (!greater(k, j)) break;
        exch pq k j -- exch(k, j)
        go j -- k = j (also acts as the "while" by recursing)

--  ***************************************************************************
--  * Haskell helper functions.
--  ***************************************************************************

modifyMutVar
  :: PrimMonad m
  => MV.MutVar (PrimState m) a
  -> (a -> a)
  -> m a
modifyMutVar mv f =
  MV.atomicModifyMutVar' mv (\n -> let !n' = f n in (n', n'))

showArray
  :: ( MArray a e m
      , Ix i
      , Show e
      )
  => a i e
  -> m String
showArray array = do
  bounds <- getBounds array
  show <$> forM (range bounds) (Arr.readArray array)
