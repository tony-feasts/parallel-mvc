module MVCSeq (solve) where

import qualified Data.IntSet as IS
import qualified Data.Sequence as Seq
import qualified Data.Vector as V

-- | Solve the Minimum Vertex Cover problem using BFS
solve :: Int -> Int -> V.Vector IS.IntSet -> IS.IntSet
solve n m adjList = bfs (Seq.singleton (IS.empty, IS.empty, 1))
    where
        bfs queue = case Seq.viewl queue of
                      Seq.EmptyL -> IS.empty  -- Suppress warning
                      (subset, cover, idx) Seq.:< subsets
                          | IS.size cover == m -> subset
                          | otherwise ->
                              bfs (enqueueNextSubsets
                                (subsets, idx) subset cover)

        -- Add all possible ways to extend a subset to the queue
        enqueueNextSubsets (subsets, idx) subset cover
            | idx > n = subsets
            | otherwise =
                enqueueNextSubsets
                    (processVertex subsets idx subset cover) subset cover

        -- Add a subset extension to the queue
        processVertex subsets v subset cover =
            let newSubset = IS.insert v subset
                newCover = IS.foldl' (\acc u -> IS.insert (encodeEdge v u) acc) 
                     cover (adjList V.! v)

            in if IS.size newCover == m
               then ((Seq.singleton (newSubset, newCover, v + 1)), n + 1)
               else ((subsets Seq.|> (newSubset, newCover, v + 1)), v + 1)

        -- Encode an edge as a single integer
        encodeEdge u v
            | u < v     = u * 10000 + v
            | otherwise = v * 10000 + u