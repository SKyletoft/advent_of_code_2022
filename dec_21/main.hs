import qualified Data.Map as M
import Data.Function.Memoize (memoize, Memoizable)
import Data.Map ((!))
import Debug.Trace (trace, traceShowId)

type Map k v = M.Map k v
data Operator = Add | Sub | Mul | Div deriving (Show, Eq)
data Monkey = Val Integer | Op String String Operator deriving (Show, Eq)

main = interact
  $ (++ "\n")
  . show
  . (\x -> (part1 x, part2 x))
  . M.fromList
  . map parse
  . lines

toFunc Add = (+)
toFunc Sub = (-)
toFunc Mul = (*)
toFunc Div = div

invFunc Add = (-)
invFunc Sub = (+)
invFunc Mul = div
invFunc Div = (*)

parse :: String -> (String, Monkey)
parse x = case words x of
  [name, lhs, op, rhs] ->
    let name' = take 4 name
    in case op of
      "+" -> (name', Op lhs rhs Add)
      "-" -> (name', Op lhs rhs Sub)
      "*" -> (name', Op lhs rhs Mul)
      "/" -> (name', Op lhs rhs Div)
      _ -> error $ "bad parse " ++ x
  [name, val] -> (take 4 name, Val $ read val)
  _ -> error $ "bad parse " ++ x

part1 m = x
  where (MVal x) = constantFold . buildTree' m $ "root"

data MathTree = X | MVal Integer | MOp Operator MathTree MathTree deriving (Show, Eq)

buildTree :: Map String Monkey -> String -> MathTree
buildTree _ "humn" = X
buildTree m n = case m ! n of
  Val n -> MVal n
  Op l r op -> MOp op (buildTree m l) (buildTree m r)

buildTree' :: Map String Monkey -> String -> MathTree
buildTree' m n = case m ! n of
  Val n -> MVal n
  Op l r op -> MOp op (buildTree' m l) (buildTree' m r)

constantFold :: MathTree -> MathTree
constantFold (MVal x) = MVal x
constantFold X = X
constantFold (MOp op (MVal x) (MVal y)) = MVal $ toFunc op x y
constantFold (MOp op x y) =
  let
    left = constantFold x
    right = constantFold y
  in case (left, right) of
    (MVal a, MVal b) -> MVal $ toFunc op a b
    _ -> MOp op left right

breakOut :: Integer -> MathTree -> Integer
breakOut l expr = case expr of
  X -> l
  MVal r | l == r -> r
  MOp Add (MVal r) x -> breakOut (l - r) x
  MOp Add x (MVal r) -> breakOut (l - r) x
  MOp Mul (MVal r) x -> breakOut (l `div` r) x
  MOp Mul x (MVal r) -> breakOut (l `div` r) x
  MOp Sub (MVal r) x -> breakOut (l - r) x
  MOp Sub x (MVal r) -> breakOut (l + r) x
  MOp Div (MVal r) x -> breakOut (l `div` r) x
  MOp Div x (MVal r) -> breakOut (l * r) x
  _ -> error "Invalid state"

part2 :: Map String Monkey -> Integer
part2 m = breakOut answer tree
  where
    MOp _ left right = constantFold . buildTree m $ "root"
    (answer, tree) = case (left, right) of
      (MVal a, b) -> (a, b)
      (a, MVal b) -> (b, a)
      _ -> error "Incorrect tree"
