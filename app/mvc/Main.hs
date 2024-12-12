-- stack exec mvc -- +RTS -l -N -A128M -RTS 20 190 50

module Main (main) where

import System.Environment (getArgs, getProgName)
import Text.Read (readMaybe) -- document readMaybe
import System.Exit (die)
import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as IS -- document IntSet
import qualified Data.Vector.Mutable as MV -- document Mutable Vector
import Control.Monad.ST -- document ST
import qualified Data.Vector as V
import Data.Char (digitToInt)  -- document digitToInt
import qualified MVCSeq as MVCS
import qualified MVCPar as MVCP
import SubsetTools (choose, nthSubsetIO, next)
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.IORef (IORef, newIORef, readIORef)

-- | Parse edge file and construct adjacency list and edge set
buildGraph :: Int -> B.ByteString -> V.Vector IS.IntSet
buildGraph n file = runST $ do
    adjList <- MV.replicate (n + 1) IS.empty

    let scan first cur bs
            | B.null bs = return ()
            | B.head bs == ' ' =
                scan cur 0 (B.tail bs)
            | B.head bs == '\n' =
                do
                    MV.modify adjList (IS.insert cur) first
                    MV.modify adjList (IS.insert first) cur
                    scan 0 0 (B.tail bs)
            | otherwise =
                scan first (cur * 10 + digitToInt (B.head bs)) (B.tail bs)

    _ <- scan 0 0 file

    return =<< V.freeze adjList

printCover :: IORef IS.IntSet -> IO ()
printCover coverRef = do
    coverSet <- readIORef coverRef
    let edgesList = IS.toList coverSet
        decode e = let u = e `div` 10000
                       v = e `mod` 10000
                   in (u, v)
        decodedEdges = map decode edgesList
    putStrLn $ "Cover Edges: " ++ show decodedEdges

printSubset :: VUM.IOVector Int -> IO ()
printSubset mv = do
    elems <- mapM (VUM.read mv) [0 .. VUM.length mv - 1]
    putStrLn $ "Subset: " ++ show elems



-- | Read file, build graph, and solve mvc
main :: IO ()
main = do
    args <- getArgs
    case args of
        (mode:nStr:mStr:cStr:[]) -> do
            case (readMaybe nStr :: Maybe Int, readMaybe mStr :: Maybe Int, readMaybe cStr :: Maybe Int) of
                (Just n, Just m, Just c) -> do
                    let path = "data/v" ++ nStr ++ "e" ++ mStr ++ ".txt"
                    file <- B.readFile path
                    let adjList = buildGraph n file
                    print $ MVCP.solve n m c adjList

                    case mode of
                        "MVCS" -> print $ IS.toList (MVCS.solve n m adjList)
                        "MVCP" -> print $ IS.toList (MVCP.solve n m c adjList)
                        _        -> die "Mode must be 'MVCS', 'MVCP', or other available approaches"

                _ -> die "n, m & c must be integers"

        (mode:nStr:mStr:[]) -> do
            case (readMaybe nStr :: Maybe Int, readMaybe mStr :: Maybe Int) of
                (Just n, Just m) -> do
                    let path = "data/v" ++ nStr ++ "e" ++ mStr ++ ".txt"
                    file <- B.readFile path
                    let adjList = buildGraph n file

                    case mode of
                        "MVCS" -> print $ IS.toList (MVCS.solve n m adjList)
                        _        -> die "Mode must be 'MVCS' for this input format"

                _ -> die "n and m must be integers"

        _ -> do
            pn <- getProgName
            die $ "Usage: " ++ pn ++ " <n> <m> <c>"

            die $ "Usage: " ++ pn ++ " <mode> <n> <m> [<c>]"
