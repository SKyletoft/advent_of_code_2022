import Data.List.Split ( oneOf, split, splitOn )
import Data.List (sort, elemIndex)
import Debug.Trace (traceShowId)


data List = List [List] | Num Int deriving (Show, Eq)

instance Ord List where
  compare (Num a) (Num b) = compare a b
  compare (List a) (List b) = compare a b
  compare (Num a) (List b) = compare (List [Num a]) (List b)
  compare (List a) (Num b) = compare (List a) (List [Num b])

parseLine :: [String] -> ([List], [String])
parseLine [] = ([], [])
parseLine ("[":xs) = (List inner:rest, end)
  where
    (inner, remaining) = parseLine xs
    (rest, end) = parseLine remaining
parseLine ("]":xs) = ([], xs)
parseLine (num:xs) = (n:rest, end)
  where
    n = Num . read $ num
    (rest, end) = parseLine xs

parse :: String -> List
parse
  = head
  . fst
  . parseLine
  . filter (`notElem` ["", ",", "\n", "\n\n"])
  . split (oneOf ",[]")

part1 :: String -> Int
part1
  = sum
  . map fst
  . filter snd
  . zip [1..]
  . map ((\[a, b] -> a <= b) . map parse . lines)
  . splitOn "\n\n"

part2 :: String -> Int
part2 input = (two + 1) * (six + 1)
  where
    Just two = elemIndex (List [Num 2]) total
    Just six = elemIndex (List [Num 6]) total
    total
      = sort
      . (++ [List [Num 2], List [Num 6]])
      . map parse
      . filter (/= "")
      . splitOn "\n"
      $ input

main :: IO ()
main = interact $ (++ "\n") . show . (\x -> (part1 x, part2 x))
