import           Data.List       (transpose)
import           Data.List.Split (splitOn)
import           Data.Maybe      (isNothing)
import           Debug.Trace     (traceShow, traceShowId)

{-

ex <- readFile "example0"

input <- readFile "input"

-}
main = interact run

run = (++ "\n") . show . (\x -> (part1 x, part2 x)) . parse

-------------------------------------------------------------------------------

parseMapLine :: String -> [Maybe Char]
parseMapLine []                   = []
parseMapLine (' ':' ':' ':' ':xs) = Nothing : parseMapLine xs
parseMapLine (' ':' ':' ':xs)     = Nothing : parseMapLine xs
parseMapLine (' ':'[':c:']':xs)   = Just c : parseMapLine xs
parseMapLine ('[':c:']':xs)       = Just c : parseMapLine xs
parseMapLine _                    = error "Invalid input"

parseInstructionLine :: String -> (Int, Int, Int)
parseInstructionLine =
  (\[_, a, _, b, _, c] -> (read a, read b - 1, read c - 1)) . words

parse :: String -> ([[Maybe Char]], [(Int, Int, Int)])
parse = (\[a, b] -> (topParser a, bottomParser b)) . splitOn [""] . lines
  where
    topParser xs =
      map (dropWhile isNothing) .
      transpose . map parseMapLine . take (length xs - 1) $
      xs
    bottomParser = map parseInstructionLine

-------------------------------------------------------------------------------

part1 (crates, instructions) =
  map (unwrap . head) . foldl part1' crates $ instructions
  where
    unwrap (Just c) = c
    unwrap Nothing  = ' '

part1' :: [[Maybe Char]] -> (Int, Int, Int) -> [[Maybe Char]]
part1' crates (0, _, _) = crates
part1' crates (repeat, from, to) =
  flip part1' (repeat - 1, from, to) . move from to $ crates

move from to crates = insert from oneShort . insert to oneLonger $ crates
  where
    (moving:oneShort)
      | null tower = error . show $ (crates, tower, from)
      | otherwise = tower
      where
        tower = crates !! from
    oneLonger = moving : (crates !! to)

insert :: Int -> a -> [a] -> [a]
insert idx x xs = take idx xs ++ [x] ++ drop (idx + 1) xs

-------------------------------------------------------------------------------

part2 (crates, instructions) =
  map (unwrap . head) . foldl part2' crates $ instructions
  where
    unwrap (Just c) = c
    unwrap Nothing  = ' '

part2' :: [[Maybe Char]] -> (Int, Int, Int) -> [[Maybe Char]]
part2' crates (0, _, _) = crates
part2' crates (repeat, from, to) = insert from short . insert to longer $ crates
  where
    (moving, short) = splitAt repeat (crates !! from)
    longer = moving ++ (crates !! to)
