-- | Ultra-fast combinatorial MVC solver with memory efficient
-- subset level processing.
-- No regeneration of subsets

module ParallelV4 (solve) where

import Control.Monad.Par (runPar, parMapM)
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VUM
import System.IO.Unsafe (unsafePerformIO)
import SubsetTools (choose, nthSubsetIO, next)

-- Verify if a vertex set (chosen) covers all edges in the adjacency list
verifyVertexCover :: IS.IntSet -> V.Vector IS.IntSet -> Bool
verifyVertexCover chosen adj =
    let n = V.length adj - 1
        isCovered u v = u `IS.member` chosen || v `IS.member` chosen
        checkEdges u = IS.foldr (\v acc -> acc && isCovered u v) True
            (adj V.! u)
    in all checkEdges [1..n]

-- Directly verify if the subset chosen (stored in mv) covers all edges
isSolution :: V.Vector IS.IntSet -> VUM.IOVector Int -> IO Bool
isSolution adjList mv = do
    let k = VUM.length mv
    vs <- mapM (VUM.read mv) [0 .. k-1]
    let chosen = IS.fromList vs
    return $ verifyVertexCover chosen adjList

-- Split index ranges into chunks
splitRanges :: Integer -> Integer -> Int -> [(Integer, Integer)]
splitRanges start total size
    | start == total = []
    | otherwise =
        let end = min (start + fromIntegral size) total
        in (start, end) : splitRanges end total size

-- Process a single range of subsets in parallel
-- Returns a list which may contain a solution
processRange :: Int -> Int -> V.Vector IS.IntSet -> (Integer, Integer) ->
    [[Int]]
processRange n k adjList (start, end) = unsafePerformIO $ do
    mv <- nthSubsetIO n k start
    sol <- isSolution adjList mv
    if sol
    then do s <- mvToList mv; return [s]
    else generateAndCheck (end - start - 1) mv
    where
        generateAndCheck 0 _ = return []
        generateAndCheck c mv = do
            next n k mv
            sol <- isSolution adjList mv
            if sol
            then do s <- mvToList mv; return [s]
            else generateAndCheck (c - 1) mv

        mvToList mv = do
            let kLen = VUM.length mv
            mapM (VUM.read mv) [0 .. kLen-1]

-- BFS over k using parMapM
solve :: Int -> Int -> V.Vector IS.IntSet -> [Int]
solve n chunkSize adjList = runPar (bfs 0)
    where
        bfs k = do
            let total = choose n k
            if total == 0 then bfs (k + 1) else do
                let ranges = splitRanges 0 total chunkSize
                results <- parMapM (return . processRange n k adjList) ranges
                case concat results of
                    (sol:_) -> return sol
                    [] -> bfs (k + 1)
