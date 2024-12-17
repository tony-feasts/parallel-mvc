module ParallelV1 (solve) where

import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (using, parList, rdeepseq)

-- Generate all k-sized subsets sequentially
genSubsets :: [a] -> Int -> [[a]]
genSubsets _ 0 = [[]]
genSubsets [] _ = []
genSubsets (x:xs) k =
    let withX = map (x:) (genSubsets xs (k-1))
        withoutX = genSubsets xs k
    in withX ++ withoutX

-- Parallel subset generation with depth control
genSubsetsParallel :: Int -> [a] -> Int -> [[a]]
genSubsetsParallel depth xs k
    | k == 0      = [[]]
    | null xs     = []
    | depth <= 0  = genSubsets xs k -- Fallback to sequential
    | otherwise   = 
        case xs of
            (x:xs') ->
                let withX = map (x:) (genSubsetsParallel (depth-1) xs' (k-1))
                    withoutX = genSubsetsParallel (depth-1) xs' k
                in withX `par` (withoutX `pseq` (withX ++ withoutX))
            [] -> [] -- Surpress warning

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
        depth = 3 -- Control parallel recursion depth
        chunkSize = 1000 -- Chunks of subsets to verify in parallel
    in search 1 vertices depth chunkSize
  where
    search size vs depth chunkSize =
        let subsets = genSubsetsParallel depth vs size
            results = map checkChunk (chunk chunkSize subsets)
                        `using` parList rdeepseq
        in case concat results of
            []      -> search (size+1) vs depth chunkSize
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