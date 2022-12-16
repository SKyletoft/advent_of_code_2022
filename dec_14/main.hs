import qualified Data.HashSet as Set
import Data.List.Split
import Data.List
import Debug.Trace (trace, traceShowId)

type Coord = (Int, Int)
type S = Set.HashSet

main = interact run

run :: String -> String
run
  = (++ "\n")
  . show
  . (\x -> (part1 x, part2 x))
  . foldr1 Set.union
  . map lineToPoints
  . lines

pairs :: [a] -> [(a, a)]
pairs (a:b:xs) = (a,b):pairs (b:xs)
pairs _ = []

line :: (Coord, Coord) -> [Coord]
line ((a,b), (x,y))
  | (a, b) == (x, y) = [(a, b)]
  | b == y
    = let [f, t] = sort [a, x]
      in (f, b):line ((f + 1, b), (t, b))
  | a == x
    = let [f, t] = sort [b, y]
      in (a, f):line ((a, f + 1), (a, t))
  | otherwise = error "diagonal"

readCoord :: String -> Coord
readCoord s = read $ '(':s++")"

lineToPoints :: String -> S Coord
lineToPoints
  = Set.fromList
  . concatMap line
  . pairs
  . map readCoord
  . splitOn " -> "

fall :: Int -> Coord -> S Coord -> S Coord
fall void (x, y) walls
  | y >= void = walls 
  | (x, y) `Set.member` walls = walls
  | otherwise = case (down', left', right') of
      (False, _, _) -> fall void down walls
      (_, False, _) -> fall void left walls
      (_, _, False) -> fall void right walls
      _             -> Set.insert (x, y) walls
    where
      down = (x, y+1)
      left = (x-1, y+1)
      right = (x+1, y+1)
      down' = down `Set.member` walls
      left' = left `Set.member` walls
      right' = right `Set.member` walls

dropSand :: Int -> S Coord -> S Coord
dropSand void = fall void (500, 0)

part1 :: S Coord -> Int
part1 walls
  = length
  . takeWhile (uncurry (/=))
  . pairs
  . iterate (dropSand void)
  $ walls
  where
    void = maximum . map snd . Set.toList $ walls

part2 :: S Coord -> Int
part2 walls
  = length
  . takeWhile (not . Set.member (500, 0))
  . iterate (dropSand (void + 5))
  . Set.union floor
  $ walls
  where
    void = maximum . map snd . Set.toList $ walls
    floor = Set.fromList $ line ((0, void + 2), (1000, void + 2))
