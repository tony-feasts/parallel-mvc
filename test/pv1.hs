import qualified Data.ByteString.Char8 as B
import GraphGenerator (fileToAdjList)
import ParallelV1 (solve)

main :: IO ()
main = do
    file <- B.readFile "data/v26e38.txt"
    let adjList = fileToAdjList 26 file
    print $ solve adjList