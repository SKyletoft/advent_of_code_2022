import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace (traceShowId, trace, traceShow)

type S a = Set.Set a
type M k v = Map.Map k v
type Coord = (Int, Int)

main
  = interact
  $ (++ "\n")
  . show
  . (\x -> (part1 x, part2 x))
  . parse

parse :: String -> [Coord]
parse
  = map (\(_, x, y) -> (x, y))
  . filter (\(elf, x, y) -> elf == '#')
  . concatMap (\(x, line) -> zipWith (\y elf -> (elf, x, y)) [0..] line)
  . zip [0..]
  . lines

step :: Int -> [Coord] -> [Coord]
step turn elves
  = zipWith
    (\ a b -> (if Set.member b duplicated then a else b))
    elves
    proposals
  where
    allElves = Set.fromList elves
    proposals = map (proposedStepOffset turn allElves) elves
    duplicated = duplicates proposals

duplicates :: [Coord] -> S Coord
duplicates
  = Set.fromList
  . Map.keys
  . Map.filter (>1)
  . foldr count' Map.empty
  where
    count' :: Coord -> M Coord Int -> M Coord Int
    count' c m = case Map.lookup c m of
      Just n -> Map.insert c (n+1) m
      Nothing -> Map.insert c 1 m

proposedStepOffset :: Int -> S Coord -> Coord -> Coord
proposedStepOffset offset map pos@(a,b)
  = snd
  . head
  . filter (\(f, _) -> f pos)
  . ((\(x, y) -> none
       [ (x-1, y-1)
       , (x-1, y)
       , (x-1, y+1)
       , (x, y-1)
       , (x, y+1)
       , (x+1, y-1)
       , (x+1, y)
       , (x+1, y+1)
       ], (a, b)
     ):)
  . drop offset
  $ [ (\(x, y) -> none [(x-1, y-1), (x-1, y), (x-1, y+1)], (a-1, b))
    , (\(x, y) -> none [(x+1, y-1), (x+1, y), (x+1, y+1)], (a+1, b))
    , (\(x, y) -> none [(x-1, y-1), (x, y-1), (x+1, y-1)], (a, b-1))
    , (\(x, y) -> none [(x-1, y+1), (x, y+1), (x+1, y+1)], (a, b+1))
    -- And repeat because it's easier than module access
    , (\(x, y) -> none [(x-1, y-1), (x-1, y), (x-1, y+1)], (a-1, b))
    , (\(x, y) -> none [(x+1, y-1), (x+1, y), (x+1, y+1)], (a+1, b))
    , (\(x, y) -> none [(x-1, y-1), (x, y-1), (x+1, y-1)], (a, b-1))
    , (\(x, y) -> none [(x-1, y+1), (x, y+1), (x+1, y+1)], (a, b+1))
    , (const True, pos)
    , (error $ "No valid moves! " ++ show map ++ ", " ++ show pos, pos)
    ]
  where
    none = not . any (`Set.member` map)

showSystem :: [Coord] -> String
showSystem cs = unlines [[f (y, x) | x <- [minx..maxx]] | y <- [miny..maxy]]
  where
    minx = minimum . map fst $ cs
    maxx = maximum . map fst $ cs
    miny = minimum . map snd $ cs
    maxy = maximum . map snd $ cs
    set  = Set.fromList cs
    f pos
      | Set.member pos set = '#'
      | otherwise = '.'

empties :: [Coord] -> Int
empties cs = abs (maxx - minx) * abs (maxy - miny) - length cs
  where
    minx = minimum . map fst $ cs
    maxx = (+1) . maximum . map fst $ cs
    miny = minimum . map snd $ cs
    maxy = (+1) . maximum . map snd $ cs

part1 :: [Coord] -> Int
part1
  = empties
  . foldl1 (.) [step (mod x 4) | x <- reverse [0..9]]

pairs (a:b:xs) = (a,b):pairs (b:xs)
pairs _ = []

part2 :: [Coord] -> Int
part2 xs
  = (+1)
  . length
  . takeWhile (uncurry (/=))
  . pairs
  . scanl (flip step) xs
  . map (`mod` 4)
  $ [0..]
