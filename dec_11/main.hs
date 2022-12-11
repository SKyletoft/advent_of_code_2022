import           Data.List.Extra (trim)
import           Data.List.Split (splitOn)
import           Debug.Trace     (traceShowId, trace)
import Data.List (sort)

{-

input <- readFile "input"

ex <- readFile "example0"

-}

data Monkey =
  Monkey
    { startingItems :: [Int]
    , op            :: Int -> Int
    , test          :: Int
    , ifTrue        :: Int
    , ifFalse       :: Int
    , inspected     :: Int
    }

parse :: [String] -> Monkey
parse xs = Monkey startingItems op test ifTrue ifFalse 0
  where
    [_, start', op', test', true', false'] = xs
    startingItems = map read . splitOn ", " . drop 16 . trim $ start'
    op =
      let [opLhs, opOp, opRhs] = words . drop 17 . trim $ op'
          opOp' =
            case opOp of
              "+" -> (+)
              "*" -> (*)
              _   -> error "Invalid operator"
       in case (opLhs, opRhs) of
            ("old", "old") -> (\x -> opOp' x x)
            ("old", a)     -> (\x -> opOp' x (read a))
            (a, "old")     -> (\x -> opOp' (read a) x)
            (a, b)         -> (\_ -> opOp' (read a) (read b))
    test = read . drop 19 . trim $ test'
    ifTrue = read . drop 25 . trim $ true'
    ifFalse = read . drop 26 . trim $ false'

insert :: Int -> a -> [a] -> [a]
insert idx x xs = take idx xs ++ [x] ++ drop (idx + 1) xs

main :: IO ()
main = interact run

run :: String -> String
run
  = (++ "\n")
  . show
  . (\x -> (part1 x, part2 x))
  . map parse
  . splitOn [""]
  . lines

turn :: [Monkey] -> Int -> [Monkey]
turn ms n = new
  where
    idx = mod n (length ms)
    (Monkey items op test ifTrue ifFalse inspected) = ms !! idx
    (item:fromItems) = items
    newWorry = div (op item) 3
    next
      | mod newWorry test == 0 = ifTrue
      | otherwise = ifFalse
    fromMonkey = Monkey fromItems op test ifTrue ifFalse (inspected + 1)
    (Monkey toItems a b c d e) = ms !! next
    toMonkey = Monkey (toItems ++ [newWorry]) a b c d e
    new = insert next toMonkey . insert idx fromMonkey $ ms

perMonkey :: [Monkey] -> Int -> [Monkey]
perMonkey ms n
  | n >= length ms = ms
  | null . startingItems . (!! n) $ ms = perMonkey ms (n + 1)
  | otherwise = perMonkey (turn ms n) n

showWorries = map startingItems

part1 :: [Monkey] -> Int
part1
  = (\[a, b] -> a * b)
  . take 2
  . reverse
  . sort
  . map inspected
  . (!! 20)
  . iterate (`perMonkey` 0)

turn2 :: [Monkey] -> Int -> [Monkey]
turn2 ms idx = new
  where
    denominator = product . map test $ ms
    (Monkey (worry:fromItems) op test' ifTrue ifFalse inspected) = ms !! idx
    newWorry = op worry `mod` denominator
    next
      | mod newWorry test' == 0 = ifTrue
      | otherwise = ifFalse
    fromMonkey = Monkey fromItems op test' ifTrue ifFalse (inspected + 1)
    (Monkey toItems a b c d e) = ms !! next
    toMonkey = Monkey (toItems ++ [newWorry]) a b c d e
    new = insert idx fromMonkey . insert next toMonkey $ ms

perMonkey2 :: [Monkey] -> Int -> [Monkey]
perMonkey2 ms n
  | n >= length ms = ms
  | null . startingItems . (!! n) $ ms = perMonkey2 ms (n + 1)
  | otherwise = perMonkey2 (turn2 ms n) n

part2 :: [Monkey] -> Int
part2
  = (\[a, b] -> a * b)
  . take 2
  . reverse
  . sort
  . map inspected
  . (!! 10000)
  . iterate (`perMonkey2` 0)
