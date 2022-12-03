import           Data.Char       (isAsciiLower, isAsciiUpper)
import           Data.List       (sort)
import           Data.List.Split (chunksOf)

main = interact run

run = (++ "\n") . show . (\x -> (part1 x, part2 x)) . lines

prio :: Char -> Int
prio x
  | isAsciiLower x = fromEnum x - fromEnum 'a' + 1
  | isAsciiUpper x = fromEnum x - fromEnum 'A' + 27
  | otherwise = error "invalid"

part1 = sum . map (head . finder . splitHelp . map prio)
  where
    splitHelp xs = splitAt (div (length xs) 2) xs
    finder (l, r) = filter (`elem` r) l

part2 =
  sum .
  map (\[a, b, c] -> prio . head . curry finder c . finder $ (a, b)) .
  chunksOf 3
  where
    finder (a, b) = filter (`elem` a) b
