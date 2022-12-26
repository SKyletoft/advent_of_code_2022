{-# LANGUAGE TupleSections #-}

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Heap as Heap
import Debug.Trace (traceShowId, trace)

type Coord = (Int, Int)
type S a = Set.Set a
type M k v = Map.Map k v
type H a = Heap.MinHeap a
type Wind = (Int, Int, Direction)

data Direction
  = N
  | S
  | W
  | E
  deriving (Show, Eq, Ord)

main
  = interact
  $ (++ "\n")
  . show
  . part1
  . parse

filterJust :: [Maybe a] -> [a]
filterJust []           = []
filterJust (Just x:xs)  = x : filterJust xs
filterJust (Nothing:xs) = filterJust xs

parseWinds :: String -> S Wind
parseWinds = Set.fromList . filterJust . concatMap perLine . zip [0 ..] . lines
  where
    perLine (y, l) = zipWith (curry (f y)) [0 ..] l
    f b (a, d) =
      case d of
        '>' -> Just (a, b, E)
        '<' -> Just (a, b, W)
        '^' -> Just (a, b, N)
        'v' -> Just (a, b, S)
        _   -> Nothing

parse :: String -> (Int, Int, S Wind)
parse s = (x, y, winds)
  where
    x = length . head . lines $ s
    y = length . lines $ s
    winds = parseWinds s

stepWind :: Int -> Int -> Wind -> Wind
stepWind maxX maxY (x, y, dir) =
  case dir of
    N | y >= 2 -> (x, y - 1, dir)
      | otherwise -> (x, maxY - 1, dir)
    S | y <= maxY - 3 -> (x, y + 1, dir)
      | otherwise -> (x, 1, dir)
    W | x >= 2 -> (x - 1, y, dir)
      | otherwise -> (maxX - 1, y, dir)
    E | x <= maxX - 3 -> (x + 1, y, dir)
      | otherwise -> (1, y, dir)

pairs (x:y:xs) = (x,y):pairs (y:xs)
pairs _ = []

blockades :: Int -> Int -> S Wind -> [S Wind]
blockades x y = iterate (Set.map (stepWind x y))

part1 :: (Int, Int, S Wind) -> Int
part1 (x, y, winds)
  = dijkstra
      neighbourFunc
      (x - 2, y - 1)
      Set.empty
      (Heap.fromList [(0, (1, 0))])
  where
    winds'
      = map (Set.map tuple3To2)
      . drop 1
      . blockades x y
      $ winds
    tuple3To2 (a, b, _) = (a, b)
    neighbourFunc = getNeighbours x y winds'

getNeighbours :: Int -> Int -> [S Coord] -> (Int, Coord) -> [(Int, Coord)]
getNeighbours maxx maxy windsOverTime (gen, (x, y))
  = map (gen + 1,)
  . filter (not . isWind)
  . filter (not . isOutOfBounds)
  $ [ (x + 1, y)
    , (x - 1, y)
    , (x, y + 1)
    , (x, y - 1)
    , (x, y)
    ]
  where
    winds = windsOverTime !! gen
    isWind = (`Set.member` winds)
    isOutOfBounds (a, b)
      = (a, b) /= (maxx - 2, maxy - 1)
        && (
          a < 1
          || b < 1
          || a > maxx
          || b > maxy
          )

dijkstra
  :: ((Int, Coord) -> [(Int, Coord)])
  -> Coord
  -> S (Int, Coord)
  -> H (Int, Coord)
  -> Int
dijkstra neighbourGen end visited unvisited
  = let
        Just (current@(gen, here), unvisited') = Heap.view unvisited
        visited' = Set.insert current visited
        unvisited''
          = Heap.union unvisited'
          . Heap.fromList
          . filter (not . (`Set.member` visited))
          . trace (show (current, unvisited'))
          . neighbourGen
          $ current 
      in if here == end
         then gen
         else dijkstra neighbourGen end visited' unvisited''
