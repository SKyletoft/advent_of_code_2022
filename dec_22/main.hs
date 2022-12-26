{-# LANGUAGE TupleSections #-}
import qualified Data.Set as Set
import Data.List.Split (oneOf, split)
import Data.List.Extra (notNull)
import Prelude hiding (Left, Right)

type S a = Set.Set a
type Coord = (Int, Int)

data Instruction = Left | Right | Forwards Int deriving (Show, Eq)
data Direction = N | S | E | W deriving (Show, Eq)

main
  = interact
  $ (++ "\n")
  . show
  . parse

parse :: String -> (S Coord, [Instruction])
parse input = (parseMap map, parseInstructions instructions)
  where
    instructions:map' = reverse . lines $ input
    map = reverse map'

parseInstructions :: String -> [Instruction]
parseInstructions = map f . filter notNull . (split . oneOf) "LR"
  where
    f "L" = Left
    f "R" = Right
    f x = Forwards $ read x

parseMap :: [String] -> (S Coord, S Coord)
parseMap
  = Set.fromList
  . concatMap perLine
  . zip [0..]
  where
    perLine (idx, chars)
      = map ((idx,) . fst)
      . filter ((== '.') . snd)
      . zip [0..]
      $ chars

turnLeft d = case d of
  N -> E
  E -> S
  S -> W
  W -> N

turnRight d = case d of
  N -> W
  E -> N
  S -> E
  W -> E

-- walk :: S Coord -> 
