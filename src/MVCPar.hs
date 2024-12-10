-- module MVCPar (solve) where

-- import qualified Data.IntSet as IS
-- import qualified Data.Sequence as Seq
-- import qualified Data.Vector as V
-- import Control.Monad.Par (runPar, parMapM)

-- -- | Solve the Minimum Vertex Cover problem using Parallel BFS
-- solve :: Int -> Int -> Int -> V.Vector IS.IntSet -> IS.IntSet
-- solve n m chunkSize adjList =
--     runPar $ bfs (Seq.singleton (IS.empty, IS.empty, 1))
--     where
--         -- Divide the queue into chunks and process them in parallel
--         bfs Seq.Empty = return IS.empty  -- Surpress warning
--         bfs queue = do
--             let totalStates = Seq.length queue
--                 ranges = splitRanges 0 totalStates chunkSize
--              --[(Seq (subset, cover, idx), [[Int]]), ...]
--             results <- parMapM (processRange queue) ranges
--             case concatMap snd results of
--                 (solution:_) -> return solution
--                 [] -> do
--                     let newQueue = foldl (Seq.><) Seq.empty (map fst results)
--                     bfs newQueue

--         -- Process a chunk of the queue
--         processRange queue (start, end) =
--             processRange' (start, Seq.empty, [])
--           where
--               processRange' (i, newSeg, solutions)
--                   | i >= end = return (newSeg, solutions)
--                   | otherwise =
--                       let (subset, cover, idx) = Seq.index queue i
--                       in  processRange' (enqueueNextSubsets
--                         (idx, newSeg, solutions) subset cover i)

--         -- Add all possible ways to extend a subset to the queue
--         enqueueNextSubsets (idx, newSeg, solutions) subset cover i
--             | idx > n = (i + 1, newSeg, solutions)
--             | idx == 0 = (i + n, newSeg, solutions)
--             | otherwise =
--                 enqueueNextSubsets
--                     (processVertex idx newSeg solutions subset cover)
--                         subset cover i

--         -- Add a subset extension to the queue
--         processVertex v newSeg solutions subset cover =
--             let newSubset = IS.insert v subset
--                 newCover = IS.foldl' (\acc u -> IS.insert (encodeEdge v u) acc)
--                     cover (adjList V.! v)
--             in if IS.size newCover == m
--                   then (0, Seq.singleton (newSubset, newCover, v + 1),
--                     newSubset : solutions)
--                   else (v + 1, newSeg Seq.|> (newSubset, newCover, v + 1),
--                     solutions)

--         -- Split index ranges into chunks
--         splitRanges start total size
--             | start >= total = []
--             | otherwise = let end = min (start + size) total
--                           in (start, end) : splitRanges end total size
        
--         -- Encode an edge as a single integer
--         encodeEdge u v
--             | u < v     = u * 10000 + v
--             | otherwise = v * 10000 + u

module MVCPar (solve) where

import qualified Data.IntSet as IS
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import Control.Monad.Par (runPar, parMapM)

-- | Solve the Minimum Vertex Cover problem using Parallel BFS
solve :: Int -> Int -> Int -> V.Vector IS.IntSet -> IS.IntSet
solve n m chunkSize adjList =
    runPar $ bfs (Seq.singleton (IS.empty, 1))
    where
        -- Divide the queue into chunks and process them in parallel
        bfs Seq.Empty = return IS.empty  -- Surpress warning
        bfs queue = do
            let totalStates = Seq.length queue
                ranges = splitRanges 0 totalStates chunkSize
             --[(Seq (subset, idx), [[Int]]), ...]
            results <- parMapM (processRange queue) ranges
            case concatMap snd results of
                (solution:_) -> return solution
                [] -> do
                    let newQueue = foldl (Seq.><) Seq.empty (map fst results)
                    bfs newQueue

        -- Process a chunk of the queue
        processRange queue (start, end) =
            processRange' (start, Seq.empty, [])
            where
                processRange' (i, newSeg, solutions)
                    | i >= end = return (newSeg, solutions)
                    | otherwise =
                        let (subset, idx) = Seq.index queue i
                        in  processRange' (enqueueNextSubsets
                            (idx, newSeg, solutions) subset i)
    
        -- Add all possible ways to extend a subset to the queue
        enqueueNextSubsets (idx, newSeg, solutions) subset i
            | idx > n = (i + 1, newSeg, solutions)
            | idx == 0 = (i + n, newSeg, solutions)
            | otherwise =
                enqueueNextSubsets
                    (processVertex idx newSeg solutions subset) subset i
        
        -- Add a subset extension to the queue
        processVertex v newSeg solutions subset =
            let newSubset = IS.insert v subset
            in if coverSize newSubset == m
                then (0, Seq.singleton (newSubset, v + 1),
                    newSubset : solutions)
                else (v + 1, newSeg Seq.|> (newSubset, v + 1),
                    solutions)
        
        -- Calculate the size of a subset's cover
        coverSize subset = IS.size $ IS.foldl' addEdges IS.empty subset
        
        addEdges acc v = IS.foldl'
            (\acc' u -> IS.insert (encodeEdge v u) acc')
                acc (adjList V.! v)
        
        -- Split index ranges into chunks
        splitRanges start total size
            | start >= total = []
            | otherwise = let end = min (start + size) total
                            in (start, end) : splitRanges end total size
        
        -- Encode an edge as a single integer
        encodeEdge u v
            | u < v     = u * 10000 + v
            | otherwise = v * 10000 + u
