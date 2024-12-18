-- stack test :pv4 --ta "+RTS -l -N8 -A128M -RTS"
import qualified Data.ByteString.Char8 as B
import GraphGenerator (fileToAdjList)
import ParallelV4 (solve)

main :: IO ()
main = do
    file <- B.readFile "data/v24e276.txt"
    let adjList = fileToAdjList 26 file
    print $ solve adjList 10