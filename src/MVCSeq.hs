module MVCSeq (solve) where

import qualified Data.Set as S
import qualified Data.Sequence as Seq -- document Seq
import qualified Data.Vector as V

-- | Solve the Minimum Vertex Cover problem using BFS
solve :: Int -> Int -> V.Vector [Int] -> [Int]
solve n m adjList = bfs (Seq.singleton ([], S.empty, 1))
    where bfs queue = case Seq.viewl queue of
                        Seq.EmptyL -> [] -- Surpress warning
                        (subset, cover, idx) Seq.:< subsets
                              | S.size cover == m -> subset
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
              let newSubset = v : subset
                  newCover = insertEdges cover (adjList V.! v)
                  insertEdges edges [] = edges
                  insertEdges edges (u:us) =
                    insertEdges (S.insert (min v u, max v u) edges) us

              in if S.size newCover == m
                 then ((Seq.singleton (newSubset, newCover, v + 1)), (n + 1))
                 else ((subsets Seq.|> (newSubset, newCover, v + 1)), (v + 1))
-- module MVCSeq (solve) where

-- import qualified Data.IntSet as IS
-- import qualified Data.Sequence as Seq
-- import qualified Data.Vector as V

-- -- Edge encoding function
-- encodeEdge :: Int -> Int -> Int
-- encodeEdge u v
--     | u < v     = u * 10000 + v
--     | otherwise = v * 10000 + u

-- -- | Solve the Minimum Vertex Cover problem using BFS
-- solve :: Int -> Int -> V.Vector [Int] -> [Int]
-- solve n m adjList = bfs (Seq.singleton ([], IS.empty, 1))
--     where
--         bfs queue = case Seq.viewl queue of
--                       Seq.EmptyL -> []  -- Suppress warning
--                       (subset, cover, idx) Seq.:< subsets
--                           | IS.size cover == m -> subset
--                           | otherwise ->
--                               bfs (enqueueNextSubsets (subsets, idx) subset cover)

--         -- Add all possible ways to extend a subset to the queue
--         enqueueNextSubsets (subsets, idx) subset cover
--             | idx > n = subsets
--             | otherwise =
--                 enqueueNextSubsets
--                     (processVertex subsets idx subset cover) subset cover

--         -- Add a subset extension to the queue
--         processVertex subsets v subset cover =
--             let newSubset = v : subset
--                 newCover = insertEdges cover v (adjList V.! v)
--                 insertEdges edges u [] = edges
--                 insertEdges edges u (w:ws) =
--                     let edge = encodeEdge u w
--                         edges' = IS.insert edge edges
--                     in insertEdges edges' u ws
--             in if IS.size newCover == m
--                then (Seq.singleton (newSubset, newCover, v + 1), n + 1)
--                else (subsets Seq.|> (newSubset, newCover, v + 1), v + 1)
