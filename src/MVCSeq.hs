module MVCSeq (solve) where

import qualified Data.Set as S
import qualified Data.Sequence as Seq -- document Seq
import qualified Data.Vector as V
import Data.Sequence (Seq(..))

-- | Solve the Minimum Vertex Cover problem using BFS
solve :: Int -> Int -> V.Vector [Int] -> [Int]
solve n m adjList = bfs (Seq.singleton ([], S.empty, 1))
    where bfs Seq.Empty = []  -- surpress warning
          bfs ((subset, cover, idx) :<| subsets)
              | S.size cover == m = subset
              | otherwise =
                bfs (enqueueNextSubsets (subsets, idx) subset cover)
  
          -- Add all possible next states to the queue
          enqueueNextSubsets (subsets, idx) subset cover
              | idx > n = subsets
              | otherwise =
                enqueueNextSubsets
                    (processVertex subsets idx subset cover) subset cover
  
          -- Add the next state for a subset to the queue
          processVertex subsets v subset cover =
              let newSubset = v : subset
                  newCover = insertEdges cover (adjList V.! v)
                  insertEdges edges [] = edges
                  insertEdges edges (u:us) =
                    insertEdges (S.insert (min v u, max v u) edges) us
  
              in if S.size newCover == m
                 then ((Seq.singleton (newSubset, S.empty, 0)), (n + 1))
                 else ((subsets Seq.|> (newSubset, newCover, v + 1)), (v + 1))
