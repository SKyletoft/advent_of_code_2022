import qualified Data.Set as Set
import Data.List (nub, transpose)
import Debug.Trace

type Coord = (Int, Int)

main :: IO ()
main
  = interact
  $ (++ "\n")
  . show
  . (\x -> (part1 x, part2 x))
  . concatMap parse
  . lines

vadd :: Coord -> Coord -> Coord
vadd (a, b) (x, y) = (a + x, b + y)

vsub :: Coord -> Coord -> Coord
vsub (a, b) (x, y) = (a - x, b - y)

vabs :: Coord -> Coord
vabs (x, y) = (abs x, abs y)

vdist :: Coord -> Int
vdist = uncurry (+) . vabs

direction :: String -> Coord
direction "U" = ( 1,  0)
direction "D" = (-1,  0)
direction "L" = ( 0, -1)
direction "R" = ( 0,  1)
direction _   = error "Bad input"

parse :: String -> [Coord]
parse line = replicate repeats dir
  where
    [a, b] = words line
    repeats = read b
    dir = direction a

move :: Coord -> Coord -> Coord
move h t = h `vadd` diff'
  where
    diff = t `vsub` h
    diff' = case diff of
      (-2, -2) -> (-1, -1)
      (-2,  2) -> (-1,  1)
      ( 2,  2) -> ( 1,  1)
      ( 2, -2) -> ( 1, -1)

      (-2, -1) -> (-1,  0)
      (-1, -2) -> ( 0, -1)
      ( 2, -1) -> ( 1,  0)
      ( 1, -2) -> ( 0, -1)
      (-2,  1) -> (-1,  0)
      (-1,  2) -> ( 0,  1)
      ( 2,  1) -> ( 1,  0)
      ( 1,  2) -> ( 0,  1)

      ( 2,  0) -> ( 1,  0)
      (-2,  0) -> (-1,  0)
      ( 0,  2) -> ( 0,  1)
      ( 0, -2) -> ( 0, -1)

      ( 3,  _) -> error "Too far!!"
      (-3,  _) -> error "Too far!!"
      ( _,  3) -> error "Too far!!"
      ( _, -3) -> error "Too far!!"
      _ -> diff

step :: (Coord, Coord) -> Coord -> (Coord, Coord)
step (h, t) dir = (h', t')
  where
    h' = h `vadd` dir
    t' = move h' t

pairs (x:y:xs) = (x,y):pairs (y:xs)
pairs _ = []

part1 :: [Coord] -> Int
part1
  = Set.size
  . Set.fromList
  . map snd
  . scanl step ((0, 0), (0, 0))

restore :: [(Coord, Coord)] -> [Coord]
restore = map (uncurry . flip $ vsub) . pairs . map snd

part2 :: [Coord] -> Int
part2
  = Set.size
  . Set.fromList
  . map snd
  . scanl step ((0,0),(0,0))
  . restore
  . scanl step ((0,0),(0,0))
  . restore
  . scanl step ((0,0),(0,0))
  . restore
  . scanl step ((0,0),(0,0))
  . restore
  . scanl step ((0,0),(0,0))
  . restore
  . scanl step ((0,0),(0,0))
  . restore
  . scanl step ((0,0),(0,0))
  . restore
  . scanl step ((0,0),(0,0))
  . restore
  . scanl step ((0,0),(0,0))

visualise :: Coord -> Coord -> [Coord] -> String
visualise (x1, y1) (x2, y2) points
  = unlines . reverse . transpose $ [[f x y | x <- [x1..x2]] | y <- [y1..y2]]
  where
    f x y
      | (x, y) `elem` points = '#'
      | otherwise = '·'
