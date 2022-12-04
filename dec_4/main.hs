import Data.List.Split (splitOn)
import Debug.Trace (traceShowId)

{-

input <- readFile "input"

ex <- readFile "example0"

-}

main :: IO ()
main = interact run

run :: String -> String
run = (++ "\n") . show . (\x -> (part1 x, part2 x)) . map parse . lines

parse :: String -> (Int, Int, Int, Int)
parse = (\[a,b,c,d] -> (a,b,c,d)) . map read . concatMap (splitOn ",") . splitOn "-"


part1 :: [(Int, Int, Int, Int)] -> Int
part1 = length . filter helper
  where
    helper (a,b,c,d) = (c <= a && d >= b) || (a <= c && b >= d)

part2 :: [(Int, Int, Int, Int)] -> Int
part2 = length . filter helper
  where
    isBetween val min max = val >= min && val <= max
    helper (a,b,c,d) =
      isBetween a c d
      || isBetween b c d
      || isBetween c a b
      || isBetween d a b
