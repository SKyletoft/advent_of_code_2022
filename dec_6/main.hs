import           Data.List (nub)

{-

input <- readFile "input"

ex <- readFile "example0"

-}

main :: IO ()
main = interact run

run :: String -> String
run = (++ "\n") . show . (\x -> (part1 x, part2 x))

both :: Int -> Int -> String -> Int
both count n xs
  | (== count) . length . nub . take count $ xs = n
  | length xs >= count = both count (n + 1) (drop 1 xs)
both _ _ _ = error "Failed"

part1 :: String -> Int
part1 = both 4 4

part2 :: String -> Int
part2 = both 14 14
