-- stack exec mvc -- +RTS -l -N -A128M -RTS MVCP 20 190 50

module Main (main) where

import qualified Data.IntSet as IS -- document IntSet
import Text.Read (readMaybe) -- document readMaybe
import qualified Data.ByteString.Char8 as B
import qualified MVCSeq as MVCS
import qualified MVCPar as MVCP
import qualified IncrementalParallel as MVIP
import qualified SequentialBruteForce as MVCSBF
import System.Exit (die)
import GraphGenerator (fileToAdjList)
import System.Environment (getArgs, getProgName)

-- | Read file, build graph, and solve mvc
main :: IO ()
main = do
    args <- getArgs
    case args of
        (mode:nStr:mStr:cStr:[]) -> do
            case (readMaybe nStr :: Maybe Int, readMaybe mStr :: Maybe Int,
                readMaybe cStr :: Maybe Int) of
                (Just n, Just m, Just c) -> do
                    let path = "data/v" ++ nStr ++ "e" ++ mStr ++ ".txt"
                    file <- B.readFile path
                    let adjList = fileToAdjList n file

                    case mode of
                        "MVCS" -> print $ IS.toList (MVCS.solve n m adjList)
                        "MVCP" -> print $ MVCP.solve n m c adjList
                        "MVIP" -> print $ MVIP.solve adjList c
                        "MVCSBF" -> print $ MVCSBF.solve adjList
                        _ -> die "Mode must be 'MVCS', 'MVCP', 'MVIP', \
                                  \or 'MVCSBF'"

                _ -> die "n, m & c must be integers"

        (mode:nStr:mStr:[]) -> do
            case (readMaybe nStr :: Maybe Int, readMaybe mStr :: Maybe Int) of
                (Just n, Just m) -> do
                    let path = "data/v" ++ nStr ++ "e" ++ mStr ++ ".txt"
                    file <- B.readFile path
                    let adjList = fileToAdjList n file

                    case mode of
                        "MVCS" -> print $ IS.toList (MVCS.solve n m adjList)
                        "MVCSBF" -> print $ MVCSBF.solve adjList
                        _ -> die "Mode must be 'MVCS', 'MVCSBF' \
                                  \for this input format"

                _ -> die "n and m must be integers"

        _ -> do
            pn <- getProgName
            die $ "Usage: " ++ pn ++ "<mode> <n> <m> [<c>]"
            