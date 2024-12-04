module GraphGenerator (generateGraph, writeGraph) where

import qualified Data.Vector.Mutable as V -- document IOVector
import Data.IORef (newIORef, readIORef, modifyIORef)
import Control.Monad (forM_)
import System.Random (randomRIO)
import System.IO (withFile, IOMode(WriteMode), hPutStrLn)

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
writeGraph :: Int -> Int -> V.IOVector (Int, Int) -> FilePath -> IO ()
writeGraph n m edges path = do
    withFile path WriteMode $ \handle -> do
        forM_ [0..m-1] $ \idx -> do
            (node, neighbor) <- V.read edges idx
            hPutStrLn handle $ show node ++ " " ++ show neighbor
