import qualified Data.Set as Set
import Debug.Trace

type S a = Set.Set a
type Coord = (Int, Int, Int)

parseCoord xs = read $ '(':xs++")"

parse :: String -> S Coord
parse = Set.fromList . map parseCoord . lines

main
  = interact
  $ (++ "\n")
  . show
  . (\x -> (part1 x, part2 x))
  . parse

part1 :: S Coord -> Int
part1 cs
  = sum
  . map (length . filter (not . (`Set.member` cs)) . surrounding)
  . Set.toList
  $ cs

surrounding :: Coord -> [Coord]
surrounding (x, y, z)
  = [(x + 1, y, z)
    ,(x - 1, y, z)
    ,(x, y + 1, z)
    ,(x, y - 1, z)
    ,(x, y, z + 1)
    ,(x, y, z - 1)
    ]

part2 :: S Coord -> Int
part2 cs
  = part1 cs - part1 inside
  where
    maxx = maximum . Set.map (\(x,_,_) -> x) $ cs
    maxy = maximum . Set.map (\(_,y,_) -> y) $ cs
    maxz = maximum . Set.map (\(_,_,z) -> z) $ cs
    end = (maxx, maxy, maxz)
    outside
      = expand
          end
          cs
          Set.empty
        $ Set.singleton (0,0,0)
    inside
      = Set.filter (`Set.notMember` Set.union cs outside)
      . block (0,0,0)
      $ end

expand :: Coord -> S Coord -> S Coord -> S Coord -> S Coord
expand end@(x,y,z) barrier visited xs
  | end `Set.member` visited = visited
  | otherwise = expand end barrier (Set.union visited visited') visited'
  where
    visited'
      = Set.filter (`Set.notMember` barrier)
      . Set.filter (`Set.notMember` visited)
      . Set.filter (\(x', y', z')
                    -> x' <= x
                     && x' >= 0
                     && y' <= y
                     && y' >= 0
                     && z' <= z
                     && z' >= 0)
      . Set.fromList
      . concatMap surrounding
      . Set.toList
      $ xs

block :: Coord -> Coord -> S Coord
block (a,b,c) (x,y,z)
  = Set.fromList [
      (x', y', z')
      | x' <- [a..x],
        y' <- [b..y],
        z' <- [c..z]]
