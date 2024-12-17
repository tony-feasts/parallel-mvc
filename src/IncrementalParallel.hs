module IncrementalParallel (solve) where

import qualified Data.IntSet as IS
import qualified Data.Vector as V
import Data.Bits ((.&.), shiftL, popCount)
import Control.Parallel.Strategies (parBuffer, rdeepseq, using)
import Data.Maybe (catMaybes)
import Data.List (minimumBy)
import Data.Ord (comparing)

type Vertex = Int
type Graph = V.Vector IS.IntSet

-- Generate subsets of a given size
subsetsOfSizeK :: Int -> [a] -> [[a]]
subsetsOfSizeK k xs = [ [xs !! i | i <- [0..n-1], testBit mask i] | mask <- masks ]
  where
    n = length xs
    masks = [ mask | mask <- [0..(2^n - 1)] :: [Int], popCount mask == k ]
    testBit m i = (m .&. (1 `shiftL` i)) /= 0

-- Verify if a subset is a valid vertex cover
verifyVertexCover :: [Vertex] -> [(Vertex, Vertex)] -> Bool
verifyVertexCover subset edges =
    all (\(u, v) -> u `elem` subset || v `elem` subset) edges

-- Generate edges from the adjacency list
generateEdges :: Graph -> [(Vertex, Vertex)]
generateEdges graph =
    [(u, v) | u <- [0..V.length graph - 1], v <- IS.toList (graph V.! u), u < v]

-- Find the minimum vertex cover using incremental parallel search
solve :: Graph -> Int -> Maybe [Vertex]
solve graph chunkSize =
  let vertices = [0..V.length graph - 1]
      edges = generateEdges graph
      k = length vertices

      processChunkLazy :: [Vertex] -> Maybe [Vertex]
      processChunkLazy subset =
        if verifyVertexCover subset edges then Just subset else Nothing

      checkSubsetsSize :: Int -> Maybe [Vertex]
      checkSubsetsSize size =
        let chunks = chunksOf chunkSize vertices
            processedChunks = concatMap (\chunk ->
              let subsets = subsetsOfSizeK size chunk
              in map processChunkLazy subsets `using` parBuffer 1000 rdeepseq) chunks
            validSubsets = catMaybes processedChunks
        in if null validSubsets then Nothing else Just (minimumBy (comparing length) validSubsets)

      findSolution :: Int -> Maybe [Vertex]
      findSolution size
        | size > k = Nothing
        | otherwise =
            case checkSubsetsSize size of
              Nothing -> findSolution (size + 1)
              Just validCover -> Just validCover

  in findSolution 1

-- Split list into chunks of given size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
