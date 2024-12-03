module Main (main) where

import GraphGenerator (writeGraph, generateGraph)
import System.Environment (getArgs, getProgName)
import Text.Read (readMaybe)
import System.Exit (die)
import Control.Monad (when)

-- | Generate and save a random graph with n nodes and m edges
-- Enforces constraints on n and m
main :: IO ()
main = do 
    args <- getArgs
    case args of
        [nStr, mStr] -> do
            case (readMaybe nStr, readMaybe mStr) of
                (Just n, Just m) -> do
                    when (n < 2 || n > 10000) $
                        die "n must be between 2 and 10^4"
                    when (m < 1 || m > n * (n - 1) `div` 2) $
                        die "m must be between 1 and n(n - 1)/2"
                    
                    edges <- generateGraph n m
                    let path = "data/v" ++ show n ++ "e" ++ show m ++ ".txt"
                    writeGraph n m edges path
                    putStrLn $ "Graph saved to: " ++ path

                _ -> die "n and m must be integers"

        _ -> do
            pn <- getProgName
            die $ "Usage: " ++ pn ++ " <n> <m>"