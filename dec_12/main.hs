import           Data.Set    (Set)
import qualified Data.Set
import           Debug.Trace (traceShowId)

{-

input <- readFile "input"

ex <- readFile "example0"

-}

main :: IO ()
main = interact run

run :: String -> String
run = (++ "\n") . show . (\x -> (part1 x, part2 x)) . map (map parse) . lines

parse :: Char -> Int
parse 'S' = parse 'a'
parse 'E' = parse 'z'
parse c   = fromEnum c - 97

pathFind
  :: [[Int]]
  -> [[Int]]
  -> Set (Int, Int)
  -> [(Int, Int)]
  -> (Int, Int)
  -> [(Int, Int)]
pathFind board board' visited history (x, y) = error ""
  where
    pathFind' = pathFind board board'

part1 _ = "todo"

part2 _ = "todo"
