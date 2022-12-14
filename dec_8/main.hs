import           Data.List   (transpose)
import           Debug.Trace (traceShowId)

{-

input <- readFile "input"

ex <- readFile "example0"

-}

main :: IO ()
main = interact run

run :: String -> String
run = (++ "\n") . show . (\x -> (part1 x, part2 x)) . parse

parse :: String -> [[Int]]
parse = map (map (read . (: []))) . lines

splitAround i xs = (take i xs, drop (i + 1) xs)

part1 :: [[Int]] -> Int
part1 input =
  (+ border) . length . filter id . concat $
  [[singleStep x y | x <- range] | y <- range]
  where
    input' = transpose input
    range = [1 .. (length input - 2)]
    border = 4 * (length input - 1)
    singleStep x y = any ((< height) . maximum) surroundings
      where
        height = input !! x !! y
        (left, right) = splitAround x (input' !! y)
        (top, bottom) = splitAround y (input !! x)
        surroundings = [left, right, top, bottom]

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive f (x:xs)
  | f x = x : takeWhileInclusive f xs
  | otherwise = [x]

part2 :: [[Int]] -> Int
part2 input = maximum . concat $ [[singleStep x y | x <- range] | y <- range]
  where
    input' = transpose input
    range = [1 .. (length input - 2)]
    singleStep x y = product . map takingFunction $ surroundings
      where
        height = input !! x !! y
        (left, right) = splitAround x (input' !! y)
        (top, bottom) = splitAround y (input !! x)
        surroundings = [reverse left, right, reverse top, bottom]
        takingFunction = length . takeWhileInclusive (< height)
