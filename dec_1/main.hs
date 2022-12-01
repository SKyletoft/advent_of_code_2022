import           Data.List       (sort)
import           Data.List.Split (splitOn)

parse :: String -> [[Int]]
parse = map (map read . lines) . splitOn "\n\n"

part1 :: [[Int]] -> Int
part1 = maximum . map sum

part2 :: [[Int]] -> Int
part2 = sum . take 3 . reverse . sort . map sum

runBoth :: [[Int]] -> (Int, Int)
runBoth xs = (part1 xs, part2 xs)

main = interact $ (++ "\n") . show . runBoth . parse
