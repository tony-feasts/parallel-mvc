module GraphGenerator (generateGraph, writeGraph, fileToAdjList) where

import Data.IORef (newIORef, readIORef, modifyIORef)
import Control.Monad (forM_)
import System.Random (randomRIO)
import System.IO (withFile, IOMode(WriteMode), hPutStrLn)
import qualified Data.ByteString as B
import qualified Data.Vector.Mutable as V -- document IOVector
import Control.Monad.ST (runST) -- document ST
import qualified Data.Vector.Mutable as MV -- document Mutable Vector
import qualified Data.IntSet as IS -- document IntSet
import Data.Char (digitToInt, chr) -- document digitToInt, chr
import qualified Data.Vector as V

-- | Generate a random graph with n nodes and m edges
-- Returns a vector with first m elements being the edges
generateGraph :: Int -> Int -> IO (V.IOVector (Int, Int))
generateGraph n m = do
    let maxEdges = n * (n - 1) `div` 2
    edges <- V.new maxEdges

    edgeIdx <- newIORef 0 -- document newIORef

    -- Generate all possible edges
    forM_ [1..n] $ \node -> do
        forM_ [node+1..n] $ \neighbor -> do
            idx <- readIORef edgeIdx
            V.write edges idx (node, neighbor)
            modifyIORef edgeIdx (+1)
    
    -- Randomly remove edges until we have m edges
    forM_ [1..(maxEdges - m)] $ \delim -> do
        idx <- randomRIO (0, maxEdges - delim) -- document randomRIO
        V.swap edges idx (maxEdges - delim)
    
    return edges

-- | Write a graph to a file
-- m lines are the edges "u v"
-- Document FilePath, WriteMode, withFile, hPutStrLn
writeGraph :: Int -> V.IOVector (Int, Int) -> FilePath -> IO ()
writeGraph m edges path = do
    withFile path WriteMode $ \handle -> do
        forM_ [0..m-1] $ \idx -> do
            (node, neighbor) <- V.read edges idx
            hPutStrLn handle $ show node ++ " " ++ show neighbor

-- | Parse edge file and construct adjacency list and edge set
fileToAdjList :: Int -> B.ByteString -> V.Vector IS.IntSet
fileToAdjList n file = runST $ do
    adjList <- MV.replicate (n + 1) IS.empty

    let scan first cur bs
            | B.null bs = return ()
            | chr (fromIntegral (B.head bs)) == ' ' =
                scan cur 0 (B.tail bs)
            | chr (fromIntegral (B.head bs)) == '\n' =
                do
                    MV.modify adjList (IS.insert cur) first
                    MV.modify adjList (IS.insert first) cur
                    scan 0 0 (B.tail bs)
            | otherwise =
                scan first
                    (cur * 10 + digitToInt (chr (fromIntegral (B.head bs))))
                        (B.tail bs)

    _ <- scan 0 0 file

    V.freeze adjList