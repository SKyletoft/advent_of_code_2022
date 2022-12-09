import           Data.List   (nub)
import           Debug.Trace (traceShowId)
import           Prelude     hiding (Left, Right)

{-

input <- readFile "input"

ex <- readFile "example0"

-}
type Coord = (Int, Int)

main :: IO ()
main = interact run

run :: String -> String
run = (++ "\n") . show . (\x -> (part1 x, part2 x)) . concatMap parse . lines

pull :: Coord -> Coord
pull (x, _)
  | x >= 2 = (1, 0)
  | x <= -2 = (-1, 0)
pull (_, x)
  | x >= 2 = (0, 1)
  | x <= -2 = (0, -1)
pull x = x

vadd :: Coord -> Coord -> Coord
vadd (a, b) (x, y) = (a + x, b + y)

vsub :: Coord -> Coord -> Coord
vsub (a, b) (x, y) = (a - x, b - y)

direction :: String -> Coord
direction "U" = (-1, 0)
direction "D" = (1, 0)
direction "L" = (0, -1)
direction "R" = (0, 1)
direction _   = error "Bad input"

parse :: String -> [Coord]
parse line = replicate repeats dir
  where
    [a, b] = words line
    repeats = read b
    dir = direction a

step :: Coord -> Coord -> Coord -> (Coord, Coord)
step h t dir = (next_h, next_t)
  where
    next_h = vadd h dir
    t_diff = vsub t next_h
    next_t = vadd next_h (pull t_diff)

part1 =
  length . nub . map snd . scanl (\(a, b) c -> step a b c) ((0, 0), (0, 0))

step' :: Coord -> Coord -> Coord
step' h = vadd h . pull . flip vsub h

part2 =
  length . nub
  . scanl step' (0, 0)
  . scanl step' (0, 0)
  . scanl step' (0, 0)
  . scanl step' (0, 0)
  . scanl step' (0, 0)
  . scanl step' (0, 0)
  . scanl step' (0, 0)
  . scanl step' (0, 0)
  . scanl step' (0, 0)
  . map snd . scanl (\(a, b) c -> step a b c) ((0, 0), (0, 0))
