module SequentialV1 (solve) where

import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import qualified Data.IntSet as IS
import Data.IntSet (IntSet)

-- Generate all k-sized subsets
genSubsets :: [a] -> Int -> [[a]]
genSubsets _ 0 = [[]]
genSubsets [] _ = []
genSubsets (x:xs) k = map (x:) (genSubsets xs (k-1)) ++ genSubsets xs k

-- Verify if a vertex set covers all edges in the adjacency list
verifyVertexCover :: [Int] -> Vector IntSet -> Bool
verifyVertexCover chosen adj =
    let n = V.length adj - 1
        isCovered u v = u `elem` chosen || v `elem` chosen
    in all (\u -> IS.foldl'
        (\acc v -> acc && isCovered u v) True (adj ! u)) [1..n]

-- Find a minimal vertex cover
solve :: Vector IntSet -> [Int]
solve adj =
    let n = V.length adj - 1
        vertices = [1..n]
    in search 1 vertices
  where
    search size vs =
        case filter (`verifyVertexCover` adj) (genSubsets vs size) of
            []      -> search (size + 1) vs
            (sol:_) -> sol
