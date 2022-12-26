import Data.List (elemIndex)

parseSnafu :: String -> Int
parseSnafu = foldl (\acc curr -> acc * 5 + digitValue curr) 0
  where
    unwrap (Just x) = x
    unwrap Nothing = error "Unwrapped Nothing"
    digitValue = ([2, 1, 0, -1, -2] !!) . unwrap . flip elemIndex "210-="

showSnafu :: Int -> String
showSnafu 0 = ""
showSnafu x = case mod x 5 of
  0 -> '0':showSnafu (div x 5)
  1 -> '1':showSnafu (div x 5)
  2 -> '2':showSnafu (div x 5)
  3 -> '=':showSnafu (div (x + 2) 5)
  4 -> '-':showSnafu (div (x + 1) 5)
  _ -> error "Invalid digit"

main
  = interact
  $ (++ "\n")
  . reverse
  . showSnafu
  . sum
  . map parseSnafu
  . lines
