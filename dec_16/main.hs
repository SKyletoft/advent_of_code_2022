import           Data.Function.Memoize
import           Data.List.Extra       (notNull)
import           Data.List.Split       (splitOneOf)
import           Data.Map              ((!))
import qualified Data.Map              as M
import qualified Data.Set              as S
import           Debug.Trace           (trace, traceShowId)

type Map k v = M.Map k v

type Set a = S.Set a

main = interact run

run :: String -> String
run = (++ "\n") . show . (\x -> part1 x 0 S.empty "AA") . parse

parse :: String -> Map String (Int, [String])
parse = M.fromList . map parseEntry . lines

parseEntry :: String -> (String, (Int, [String]))
parseEntry line = (name, (flow, connections))
  where
    w = splitOneOf " =;," line
    name = w !! 1
    flow = read $ w !! 5
    connections = filter notNull . drop 11 $ w

part1 :: Map String (Int, [String]) -> Int -> Set String -> String -> Int
part1 connections = f'
  where
    lc = length connections
    f' = memoize f
    f depth open at
      | depth >= 30 = 0
      | lc == S.size open = (30 - depth) * flow
      | otherwise = flow + bestNext
      where
        (_, connections') = connections ! at
        flow = sum . S.map (fst . (connections !)) $ open
        openThis
          | at `S.member` open = []
          | otherwise = map (f' (depth + 2) (S.insert at open)) connections'
        goOn = map (f' (depth + 1) open) connections'
        bestNext = maximum $ openThis ++ goOn
