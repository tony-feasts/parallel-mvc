-- stack test :pv3 --ta "+RTS -l -N -A128M -RTS" --ta "8"
import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs)
import GraphGenerator (fileToAdjList)
import ParallelV3 (solve)

main :: IO ()
main = do
 args <- getArgs
 case args of
  [depth] -> do
      let depthInt = read depth :: Int
      file <- B.readFile "data/v26e38.txt"
      let adjList = fileToAdjList 26 file
      print $ solve adjList depthInt
  _ -> putStrLn
      "Usage: stack test :pv3 --ta \"+RTS -l -N -A128M -RTS\" --ta \"depth\"" 