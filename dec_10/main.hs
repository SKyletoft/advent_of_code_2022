import           Data.List.Extra (chunksOf)
import           Debug.Trace     (traceShowId)

{-

input <- readFile "input"

ex <- readFile "example0"

-}

main :: IO ()
main = do
  input <- getContents
  let input' = concatMap parse . lines $ input
  let res1 = part1 input'
  let res2 = part2 input'
  print res1
  putStrLn res2

parse x =
  case drop 5 x of
    ""  -> [0]
    num -> [0, read num]

part1 :: [Int] -> Int
part1 xs = sum . map (\x -> x * (res !! (x-1))) $ interest
  where
    res = scanl (+) 1 xs
    interest = [20, 60, 100, 140, 180, 220]

part2 :: [Int] -> String
part2 = unlines . chunksOf 40 . zipWith f pixels . scanl (+) 1
  where
    pixels = concat . repeat $ [0 .. 39]
    f x y
      | x == y = '#'
      | (x + 1) == y = '#'
      | (x - 1) == y = '#'
      | otherwise = '.'
