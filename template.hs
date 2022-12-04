import Debug.Trace (traceShowId)

{-

input <- readFile "input"

ex <- readFile "example0"

-}

main :: IO ()
main = interact run

run :: String -> String
run = (++ "\n") . show . (\x -> (part1 x, part2 x)) . map parse . lines

parse _ = "todo"

part1 _ = "todo"

part2 _ = "todo"
