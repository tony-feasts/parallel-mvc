{- Finally we are getting somewhere. We don't separate the generation and
   verification. This means we only ever need to store as many subsets as there
   are dfs threads occuring at the same time. This is memory efficient, but
   could be more time-efficient since we build up all subsets from scratch for
   every size. -}

module ParallelV3 (solve) where

import Control.Parallel (par, pseq)
import qualified Data.IntSet as IS
import qualified Data.Vector as V

-- Verify if a vertex set covers all edges in the adjacency list
verifyVertexCover :: IS.IntSet -> V.Vector IS.IntSet -> Bool
verifyVertexCover chosen adj =
    let n = V.length adj - 1
        isCovered u v = u `IS.member` chosen || v `IS.member` chosen
        checkEdges u = IS.foldr (\v acc -> acc && isCovered u v) True
            (adj V.! u)
    in all checkEdges [1..n]

-- A sequential fallback function for when depth = 0
genAndCheckSeq :: V.Vector IS.IntSet -> Int -> [Int] -> IS.IntSet ->
    IS.IntSet
genAndCheckSeq adj k xs chosen
    | k == 0 =
        if verifyVertexCover chosen adj
        then chosen
        else IS.empty
    | null xs = IS.empty
    | otherwise =
        let (x:xs') = xs
            -- Try including x
            chosenWith = genAndCheckSeq adj (k-1) xs' (IS.insert x chosen)
        in if not (IS.null chosenWith)
           then chosenWith
           else
               -- If including x failed, try excluding x
               genAndCheckSeq adj k xs' chosen

-- Parallel search with depth control and verification at base case
genAndCheckPar :: Int -> V.Vector IS.IntSet -> Int -> [Int] ->
    IS.IntSet -> IS.IntSet
genAndCheckPar depth adj k xs chosen
    | k == 0 =
        -- Base case: we have a complete subset of size we wanted
        if verifyVertexCover chosen adj
        then chosen
        else IS.empty
    | null xs = IS.empty
    | depth == 0 =
        -- Fallback to sequential if no more parallel depth allowed
        genAndCheckSeq adj k xs chosen
    | otherwise =
        let (x:xs') = xs
            -- Try including x
            chosenWith = genAndCheckPar (depth - 1) adj (k-1) xs'
                (IS.insert x chosen)
            -- Try excluding x
            chosenWithout = genAndCheckPar (depth - 1) adj k xs' chosen
        in
            -- Evaluate chosenWith in parallel, then force chosenWithout
            -- Force evaluation of chosenWith with null check
            -- We only want one solution, we discard chosenWithout if
            -- chosenWith not null
            chosenWith `par` (chosenWithout `pseq`
                (if not (IS.null chosenWith)
                 then chosenWith
                 else chosenWithout))

-- Find a minimal vertex cover using parallelized subset generation & checking
solve :: V.Vector IS.IntSet -> Int -> [Int]
solve adj depth = solve' adj depth 1 nodes
    where
        n = V.length adj - 1
        nodes = [1..n]
        solve' adj depth size nodes =
            let sol = genAndCheckPar depth adj size nodes IS.empty
            in if IS.null sol
               then solve' adj depth (size + 1) nodes
               else IS.toList sol
            