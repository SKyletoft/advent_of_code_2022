import Debug.Trace (traceShowId)
data RCS
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Show)

score :: RCS -> Int
score Rock     = 1
score Paper    = 2
score Scissors = 3

game :: RCS -> RCS -> Int
game x@Rock Scissors = 6 + score x
game x@Scissors Paper = 6 + score x
game x@Paper Rock = 6 + score x
game x y
  | x == y = 3 + score x
  | otherwise = score x

fromLetter :: String -> RCS
fromLetter "A" = Rock
fromLetter "X" = Rock
fromLetter "B" = Paper
fromLetter "Y" = Paper
fromLetter _   = Scissors

part1 :: [[RCS]] -> Int
part1 = sum . map single
  where
    single [x, y] = game y x
    single _      = error "Bad parse"

toWin :: RCS -> RCS -> RCS
toWin x Paper = x
toWin Paper x = x
toWin Rock Rock = Scissors
toWin Rock Scissors = Paper
toWin Scissors Rock = Paper 
toWin Scissors Scissors = Rock

part2 :: [[RCS]] -> Int
part2 = sum . map single
  where
    single [x,y] = flip game x . toWin x $ y
    single _ = error "Bad parse"

both f g x = (f x, g x)

run :: String -> String
run =
  (++ "\n")
  . show
  . both part1 part2 
  . map (map fromLetter . words)
  . lines

main :: IO ()
main = interact run
