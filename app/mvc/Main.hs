-- stack exec mvc -- +RTS -l -N -A128M -RTS 20 190 10000

module Main (main) where

import qualified Data.ByteString.Char8 as B
import Text.Read (readMaybe)
import System.Exit (die)
import System.Environment (getArgs, getProgName)
import GraphGenerator (fileToAdjList)
import ParallelV4 (solve)

-- | Read file, build graph, and solve mvc
main :: IO ()
main = do
    args <- getArgs
    case args of
        (nStr:mStr:cStr:[]) -> do
            case (readMaybe nStr :: Maybe Int, readMaybe mStr :: Maybe Int, 
                readMaybe cStr :: Maybe Int) of

                (Just n, Just _, Just c) -> do
                    let path = "data/v" ++ nStr ++ "e" ++ mStr ++ ".txt"
                    file <- B.readFile path
                    let adjList = fileToAdjList n file
                    print $ solve n c adjList
                _ -> die "n, m & c must be integers"

        _ -> do
            pn <- getProgName
            die $ "Usage: " ++ pn ++ "<n> <m> [<c>]"
            