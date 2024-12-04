module Main (main) where

import System.Environment (getArgs, getProgName)
import Text.Read (readMaybe) -- document
import System.Exit (die)
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S -- document Set
import qualified Data.Vector.Mutable as MV -- document Vector
import Data.STRef -- document STRef
import Control.Monad.ST -- document ST
import qualified Data.Vector as V
import Data.Char (digitToInt)

-- | Parse edge file and construct adjacency list and edge set
buildGraph :: Int -> B.ByteString -> (V.Vector [Int], S.Set (Int, Int))
buildGraph n file = runST $ do
    edgeSet <- newSTRef S.empty
    adjList <- MV.replicate (n + 1) []

    let scan first cur bs
            | B.null bs = return ()
            | B.head bs == ' ' =
                scan cur 0 (B.tail bs)
            | B.head bs == '\n' =
                do
                    modifySTRef edgeSet (S.insert (first, cur))
                    MV.modify adjList (cur :) first
                    MV.modify adjList (first :) cur
                    scan 0 0 (B.tail bs)
            | otherwise = -- document digitToInt
                scan first (cur * 10 + digitToInt (B.head bs)) (B.tail bs)

    _ <- scan 0 0 file

    finalEdgeSet <- readSTRef edgeSet
    finalAdjList <- V.freeze adjList
    return (finalAdjList, finalEdgeSet)

-- | Read file, build graph, and solve mvc
main :: IO ()
main = do
    args <- getArgs
    case args of
        [nStr, mStr] -> do
            case (readMaybe nStr :: Maybe Int, readMaybe mStr :: Maybe Int) of
                (Just n, Just _m) -> do -- document _m
                    let path = "data/v" ++ nStr ++ "e" ++ mStr ++ ".txt"
                    file <- B.readFile path
                    let (adjList, edgeSet) = buildGraph n file
                    
                    -- Print adjacency list
                    putStrLn "Adjacency List:"
                    V.imapM_ (\i neighbors -> putStrLn $ show i ++ ": " ++ show neighbors) adjList

                    -- Print edge set
                    putStrLn "\nEdge Set:"
                    print edgeSet

                _ -> die "n and m must be integers"

        _ -> do
            pn <- getProgName
            die $ "Usage: " ++ pn ++ " <n> <m>"
