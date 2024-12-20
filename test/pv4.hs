-- stack test :pv4 --ta "+RTS -l -N -A128M -RTS" --ta "10000"
import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs)
import GraphGenerator (fileToAdjList)
import ParallelV4 (solve)

main :: IO ()
main = do
  args <- getArgs
  case args of
   [chunkSize] -> do
     let chunkSizeInt = read chunkSize :: Int
     file <- B.readFile "data/v26e38.txt"
     let adjList = fileToAdjList 26 file
     print $ solve 26 chunkSizeInt adjList
   _ -> putStrLn
    "Usage: stack test :pv4 --ta \"+RTS -l -N -A128M -RTS\" --ta \"chunkSize\""