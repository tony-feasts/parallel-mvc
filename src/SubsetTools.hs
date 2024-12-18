-- Subset tools to help us with stateless combinatorial MVC solver

module SubsetTools (choose, nthSubsetIO, next) where

import qualified Data.Vector.Unboxed.Mutable as VUM

factorial :: Int -> Integer
factorial n = product [1 .. fromIntegral n]

choose :: Int -> Int -> Integer
choose n k = factorial n `div` (factorial k * factorial (n - k))

-- | Construct the 0-indexed nth subset of size k from {1..n} directly into a
-- mutable vector in ascending order.
nthSubsetIO :: Int -> Int -> Integer -> IO (VUM.IOVector Int)
nthSubsetIO n k num = do
    mv <- VUM.new k
    let go chosenCount remain start
          | chosenCount == k = return ()
          | otherwise = do
              let c = choose (n - start) (k - 1 - chosenCount)
              if c > remain
                 then do
                   -- Choose this element and move on
                   VUM.unsafeWrite mv chosenCount start
                   go (chosenCount + 1) remain (start + 1)
                 else
                   -- Skip this element
                   go chosenCount (remain - c) (start + 1)
    go 0 num 1
    return mv

-- | Given the current subset in mv (in ascending order), produce the next
-- subset in lexicographical order in place.
-- We assume a next subset exists.
next :: Int -> Int -> VUM.IOVector Int -> IO ()
next n k mv = do
    i <- findPivot (k - 1)
    x <- VUM.unsafeRead mv i
    let x' = x + 1
    VUM.unsafeWrite mv i x'
    -- Fill subsequent elements with consecutive numbers
    let fill j
          | j >= k = return ()
          | otherwise = do
              VUM.unsafeWrite mv j (x' + (j - i))
              fill (j + 1)
    fill (i + 1)
  where
    findPivot i
      | i == k - 1 = do
          x <- VUM.unsafeRead mv i
          -- If x < n, we can increment the last element
          if x < n then return i else findPivot (i - 1)
      | otherwise = do
          x <- VUM.unsafeRead mv i
          let needed = (k - 1 - i) -- how many elements come after i
              maxVal = n - needed
          if x < maxVal
            then return i
            else findPivot (i - 1)
