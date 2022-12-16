{-# LANGUAGE NumericUnderscores #-}

import Debug.Trace (traceShowId)
import Data.List.Split (splitOneOf)
import qualified Data.HashSet as Set
import Data.List (sort, nub)

type S = Set.HashSet
type Sensor = (Int, Int, Int, Int)
type Coord = (Int, Int)

main :: IO ()
main = interact run

{-
  part1' assumes that no sensors exist outside the square drawn by part2.
  It also assumes that the one missing square of part2 isn't on line 2M.
  But in return it's just *so* much faster than the more naïve part1.
-}

run :: String -> String
run
  = (++ "\n")
  . show
  . (\x -> (part1' x, part2 x))
  . map parse
  . lines

parse :: String -> Sensor
parse l = (w !! 1, w !! 3, w !! 5, w !! 7)
  where
    w = map read . splitOneOf ",=:" $ l

range :: Sensor -> Int
range (sx, sy, bx, by) = abs (sx - bx) + abs (sy - by)

perSensor :: Int -> Sensor -> S Int
perSensor line s@(sx, sy, bx, by)
  | by == line = Set.delete bx res
  | otherwise = res 
  where
    r = range s
    heightDiff = abs $ line - sy
    maxWidthDiff = r - heightDiff + 1-- +1 for the overlap of the corner
    res = Set.fromList [(sx - maxWidthDiff + 1)..(sx + maxWidthDiff - 1)]

part1 :: [Sensor] -> Int
part1
  = Set.size
  . foldr1 Set.union
  . map (perSensor 2_000_000)

minMaxSensor :: Int -> Sensor -> (Int, Int)
minMaxSensor line s@(sx, sy, bx, by)
  = (sx - maxWidthDiff + 1, sx + maxWidthDiff - 1)
  where
    r = range s
    heightDiff = abs $ line - sy
    maxWidthDiff = r - heightDiff + 1-- +1 for the overlap of the corner

part1' :: [Sensor] -> Int
part1' xs = maximum b - minimum a
  where
    (a, b) = unzip . map (minMaxSensor 2_000_000) $ xs

isInRange :: Coord -> Sensor -> Bool
isInRange (x, y) sensor@(sx, sy, _, _) = range sensor >= range (sx, sy, x, y)

expected (x1, y1, r1) (x2, y2, r2) = (range (x1, y1, x2, y2), r1 + r2)
cart xs = [(xs !! a, xs !! b) | a <- [0..l], b <- [a..l]]
  where
    l = length xs - 1


directedOutline :: (Int, Int, Int) -> (Int, Int, Int) -> [Coord]
directedOutline (x, y, r) (x', y', _)
  | x < x' && y < y' = [(x + a, y + b) | a <- [0..r + 1], let b = r - a + 1]
  | x > x' && y < y' = [(x - a, y + b) | a <- [0..r + 1], let b = r - a + 1]
  | x < x' && y > y' = [(x + a, y - b) | a <- [0..r + 1], let b = r - a + 1]
  | x > x' && y > y' = [(x - a, y - b) | a <- [0..r + 1], let b = r - a + 1]
  | otherwise          = error "No matching direction"

sensorToRangedCoord :: Sensor -> (Int, Int, Int)
sensorToRangedCoord s@(a, b, c, d) = (a, b, range s)

isClose :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
isClose x@(x1,y1,r1) y@(x2,y2,r2)
  = dist == expected_dist + 2
  where
    dist = range (x1, y1, x2, y2)
    expected_dist = r1 + r2

part2 :: [Sensor] -> Int
part2 sensors
  = (\(a, b) -> a * 4_000_000 + b)
  . head
  . filter (\(x, y)
            -> x >= 0
            && x <= range
            && y >= 0
            && y <= range
            && (not . any (isInRange (x, y)) $ sensors))
  . concatMap (uncurry directedOutline)
  . filter (uncurry isClose)
  . cart
  . map sensorToRangedCoord
  $ sensors
  where
    range = 4_000_000
