module SequentialBruteForce 
    ( solve 
    ) where

import qualified Data.IntSet as IS
import qualified Data.Vector as V
import Data.Bits (popCount, (.&.), shiftL)
import Data.Maybe (listToMaybe)

type Vertex = Int
type Graph = V.Vector IS.IntSet

-- Verify if a subset is a valid vertex cover
verifyVertexCover :: [Vertex] -> [(Vertex, Vertex)] -> Bool
verifyVertexCover subset edges =
    all (\(u, v) -> u `elem` subset || v `elem` subset) edges

-- Generate edges from the adjacency list
generateEdges :: Graph -> [(Vertex, Vertex)]
generateEdges graph =
    [(u, v) | u <- [0..V.length graph - 1], v <- IS.toList (graph V.! u), u < v]

-- Generate all subsets of the given size
genSubsets :: [Vertex] -> Int -> [[Vertex]]
genSubsets vertices size =
    let n = length vertices
        -- Specify the type for the range of masks
        masks = [mask :: Int | mask <- [0 .. (2^n - 1)], popCount mask == size]
    in [ [vertices !! i | i <- [0..n-1], (mask .&. (1 `shiftL` i)) /= 0] | mask <- masks ]


-- Sequential (Brute Force) Algorithm
solve :: Graph -> Maybe [Vertex]
solve graph =
    let vertices = [0..V.length graph - 1]
        k = length vertices
        edges = generateEdges graph
        trySize i =
            let subsets = genSubsets vertices i
            in find (\s -> verifyVertexCover s edges) subsets
        solutions = [trySize i | i <- [1..k]]
    in listToMaybe $ concat $ filter (not . null) solutions
  where
    -- Find first subset satisfying predicate
    find :: (a -> Bool) -> [a] -> [a]
    find _ [] = []
    find p (x:xs) = if p x then [x] else find p xs
