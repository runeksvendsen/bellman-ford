{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
-- | Transaction of Robert Sedgewick and Kevin Wayne's @IndexMinPQ.java@ to Haskell.
--
-- https://algs4.cs.princeton.edu/44sp/IndexMinPQ.java.html
module Data.IndexMinPQ
( IndexMinPQ
, newIndexMinPQ, newIndexMinPQTrace
, isEmpty
, insert
, minKey
, delMin
, keyOf
, contains
, decreaseKey
, emptyAsSortedList
)
where

import Data.Graph.Prelude
import qualified Data.Primitive as MV
import qualified Data.Vector.Generic.Mutable as Vec
import qualified Data.Vector.Unboxed.Mutable as UVec
import qualified Data.Vector.Mutable as BVec
import Debug.Trace (traceM)
import qualified Control.Exception as Ex
import qualified System.IO.Unsafe as Unsafe
import Data.List (intercalate)

-- | Indexed min priority queue.
--
-- See https://algs4.cs.princeton.edu/44sp/IndexMinPQ.java.html.
data IndexMinPQ s key = IndexMinPQ
  { state_maxN :: {-# UNPACK #-} Int
    -- ^ maximum number of elements on PQ
  , state_n :: MV.MutVar s Int
    -- ^ number of elements on PQ
  , state_pq :: UVec.STVector s Int
    -- ^ binary heap using 1-based indexing
  , state_qp :: UVec.STVector s Int
    -- ^ inverse of pq - qp[pq[i]] = pq[qp[i]] = i
  , state_keys :: BVec.STVector s key
    -- ^ keys[i] = priority of i
    --
    --   NOTE: must be non-strict (since we store /bottom/ for non-existing keys).
  , indexMinPQ_trace :: Bool
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
  pq' <- showArray n (state_pq pq)
  qp <- showArray n (state_qp pq)
  keys <- showArrayWithBottoms n (state_keys pq)
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

newIndexMinPQ :: Show key => Int -> ST s (IndexMinPQ s key)
newIndexMinPQ = newIndexMinPQ' False

newIndexMinPQTrace :: Show key => Int -> ST s (IndexMinPQ s key)
newIndexMinPQTrace = newIndexMinPQ' True

newIndexMinPQ' :: Show key => Bool -> Int -> ST s (IndexMinPQ s key)
newIndexMinPQ' trace maxN = do
  -- Translation of IndexMinPQ constructor
  when (maxN < 0) $
    fail $ "newIndexMinPQ: Invalid maxN: " <> show maxN
  n <- MV.newMutVar 0
  pq <- Vec.replicate (maxN + 1) 0
  qp <- Vec.replicate (maxN + 1) (-1)
  keys <- Vec.generate (maxN + 1) mkKeysError
  let queue = IndexMinPQ
        { indexMinPQ_trace = trace
        , state_maxN = maxN
        , state_n = n
        , state_pq = pq
        , state_qp = qp
        , state_keys = keys
        }
  debugTrace queue $ "Created new IndexMinPQ of size " <> show maxN
  pure queue

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
  write pq state_qp i n -- qp[i] = n
  write pq state_pq n i -- pq[n] = i
  write pq state_keys i key -- keys[i] = key
  swim pq n -- swim(n)
  debugTrace pq $ "insert " <> show i <> " " <> show key

minIndex
  :: Show key
  => IndexMinPQ s key
  -> ST s Int
minIndex pq = do
  assertQueueNotEmpty pq
  Vec.read (state_pq pq) 1 -- return pq[1]

minKey
  :: Show key
  => IndexMinPQ s key
  -> ST s key
minKey pq = do
  minKeyIndex <- minIndex pq -- pq[1]
  Vec.read (state_keys pq) minKeyIndex -- keys[pq[1]]

delMin
  :: (Ord key, Show key)
  => IndexMinPQ s key
  -> ST s Int
delMin pq = do
  min' <- minIndex pq -- int min = pq[1]
  prevN <- MV.atomicModifyMutVar' (state_n pq) (\n -> (n-1, n)) -- decrement "n", return original "n"
  exch pq 1 prevN -- exch(1, n--) -- NOTE: the unincremented "n" is passed to "exch" since the postfix decrement operator is used
  sink pq 1 -- sink(1)
  unlessM ((== min') <$> Vec.read (state_pq pq) prevN) $
    assertFail pq "Assertion failed: min == pq[n+1]"  -- assert min == pq[n+1]
  write pq state_qp min' (-1) -- qp[min] = -1
  write pq state_keys min' $ mkKeysError min' -- keys[min] = null
  write pq state_pq prevN (-1) -- pq[n+1] = -1
  debugTrace pq "delMin"
  pure min'

-- | Get the key for the given index.
--
--  Throws an error if the index does not exist.
keyOf
  :: IndexMinPQ s key
  -> Int
  -> ST s key
keyOf pq i = do
    unlessM (contains pq i) $
      fail $ "no such index: " <> show i
    Vec.read (state_keys pq) i

contains
  :: IndexMinPQ s key
  -> Int
  -> ST s Bool
contains pq i = do
  validateIndex pq i
  (/= -1) <$> Vec.read (state_qp pq) i -- return qp[i] != -1;

decreaseKey
  :: (Ord key, Show key)
  => IndexMinPQ s key
  -> Int
  -> key
  -> ST s ()
decreaseKey pq i key = do
  validateIndex pq i
  iKey <- keyOf pq i
  when (iKey == key) $ -- if (keys[i].compareTo(key) == 0)
    fail $ "Calling decreaseKey() with a key equal to the key in the priority queue: " <> show (i, key)
  when (key > iKey) $ -- if (keys[i].compareTo(key) < 0)
    fail $ "Calling decreaseKey() with a key strictly greater than the key in the priority queue: " <> show (i, key, iKey)
  write pq state_keys i key -- keys[i] = key
  Vec.read (state_qp pq) i >>= swim pq -- swim(qp[i])
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

-- | The bottom value stored in unused indices of 'state_keys'.
--
-- In the Java version /null/ is used. But we use a value
-- with the same effect (a crash) but with a more descriptive
-- crash message.
mkKeysError :: Int -> key
mkKeysError i =
  error $ "BUG: 'state_keys' does not contain key for index: " <> show i

greater
  :: Ord key
  => IndexMinPQ s key
  -> Int
  -> Int
  -> ST s Bool
greater pq i j = do
  pqI <- Vec.read (state_pq pq) i
  pqJ <- Vec.read (state_pq pq) j
  pqIKey <- Vec.read (state_keys pq) pqI
  pqJKey <- Vec.read (state_keys pq) pqJ
  pure $ pqIKey > pqJKey

exch
  :: Show key
  => IndexMinPQ s key
  -> Int
  -> Int
  -> ST s ()
exch pq i j = do
  swap <- Vec.read (state_pq pq) i -- int swap = pq[i];
  oldPqJ <- Vec.read (state_pq pq) j -- "save the old pq[j] because we need to use it twice"
  write pq state_pq i oldPqJ -- pq[i] = pq[j];
  write pq state_pq j swap -- pq[j] = swap;
  write pq state_qp oldPqJ i -- qp[oldPqJ] = i;
  write pq state_qp swap j -- qp[swap] = j;
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

-- | Write a value to an array
write
  :: ( Vec.MVector v a
     , Show key
     )
  => IndexMinPQ s key -- ^ Queue
  -> (IndexMinPQ s key -> v s a) -- ^ Array in queue
  -> Int -- ^ Index
  -> a -- ^ Value
  -> ST s ()
write pq arr =
  Vec.write (arr pq)

modifyMutVar
  :: PrimMonad m
  => MV.MutVar (PrimState m) a
  -> (a -> a)
  -> m a
modifyMutVar mv f =
  MV.atomicModifyMutVar' mv (\n -> let !n' = f n in (n', n'))

-- | Show the given number of elements from an array starting at index 1
showArray
  :: ( Vec.MVector v a
     , PrimMonad m
     , Show a
     )
  => Int -- ^ Number of elements to show (starting from index 1)
  -> v (PrimState m) a
  -> m String
showArray n array =
  show <$> Vec.foldr (:) [] (Vec.slice 1 n array)

-- | Show the given number of elements from an array starting at index 1,
--   with 'error' being shown as "null"
showArrayWithBottoms
  :: ( Vec.MVector v a
     , PrimMonad m
     , Show a
     )
  => Int -- ^ Number of elements to show (starting from index 1)
  -> v (PrimState m) a
  -> m String
showArrayWithBottoms n array = fmap showStrLst $
  map showBottomAsNull <$> Vec.foldr (:) [] (Vec.slice 1 n array)
  where
    showStrLst :: [String] -> String
    showStrLst lst = '[' : intercalate "," lst ++ "]"

    showBottomAsNull :: Show a => a -> String
    showBottomAsNull a = Unsafe.unsafePerformIO $
      showIt <$> Ex.try (Ex.evaluate a)
      where
        showIt :: Show b => Either Ex.ErrorCall b -> String
        showIt = either (const "null") show
