module ParallelV2 (solve) where

import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import Control.Parallel.Strategies (using, parList, rdeepseq)
import qualified Data.Map as Map

-- Verify if a vertex set covers all edges in the adjacency list
verifyVertexCover :: [Int] -> Vector IntSet -> Bool
verifyVertexCover chosen adj =
    let n = V.length adj - 1
        isCovered u v = u `elem` chosen || v `elem` chosen
    in all (\u -> IS.foldl'
        (\acc v -> acc && isCovered u v) True (adj ! u)) [1..n]


-- Find a minimal vertex cover using parallelized subset generation & checking
solve :: Vector IntSet -> [Int]
solve adj =
    let n = V.length adj - 1
        vertices = [1..n]
        chunkSize = 1000 -- Chunks of subsets to verify in parallel
        cache = Map.empty -- Initial cache
    in search 1 vertices chunkSize cache
  where
    search size vs chunkSize cache =
        let (subsets, newCache) = genSubsetsCache vs size cache
            results = map checkChunk (chunk chunkSize subsets)
                        `using` parList rdeepseq
        in case concat results of
            []      -> search (size+1) vs chunkSize newCache
            (sol:_) -> sol

    -- Check all subsets in a chunk
    checkChunk subsets =
        [ s | s <- subsets, verifyVertexCover s adj ]

-- Helper: Chunk a list into fixed-size pieces
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs =
    let (h,t) = splitAt n xs
    in h : chunk n t

-- Memoized version of genSubsets with caching, using lists instead of sets
genSubsetsCache :: (Ord a) => [a] -> Int -> Map.Map ([a], Int) [[a]] ->
    ([[a]], Map.Map ([a], Int) [[a]])
-- Base case: Only the empty list for size 0
genSubsetsCache _ 0 cache = ([[]], cache)
-- No subsets if the list is empty and n > 0
genSubsetsCache [] _ cache = ([], cache)
genSubsetsCache list n cache
  -- Return cached results if available
  | Map.member (list, n) cache = (cache Map.! (list, n), cache)
  | otherwise =
      let (x:xs) = list
          -- Generate subsets with and without x
          -- subsets including x
          (withX, cache1) = genSubsetsCache xs (n - 1) cache
          -- subsets excluding x
          (withoutX, cache2) = genSubsetsCache xs n cache1
          result = (map (x:) withX ++ withoutX)
          -- Store the result in the cache
          updatedCache = Map.insert (list, n) result cache2
      in (result, updatedCache)
