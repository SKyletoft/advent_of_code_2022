{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections      #-}

import           Data.List.Split (splitOn)

{-

input <- readFile "input"

ex <- readFile "example0"

-}

main :: IO ()
main = interact run

run :: String -> String
run = (++ "\n") . show . (\x -> (part1 x, part2 x)) . parse

data Folder =
  Folder
    { subfolders :: [(String, Folder)]
    , files      :: [(String, Int)]
    }
  deriving (Eq, Show)

parse :: String -> Folder
parse = f . commands
  where
    f = snd . foldl g ([], Folder [] [])
    g :: ([String], Folder) -> [String] -> ([String], Folder)
    g (path, fs) (cmd:cmds)
      | take 3 cmd == "cd " = (cd path (drop 3 cmd), fs)
      | cmd == "ls" = (path, insertAt path fs (ls cmds))
      | otherwise = error $ "Unknown command: " ++ cmd
    g (path, _) [] = error "No command"

ls :: [String] -> Folder
ls x = Folder subfolders files
  where
    isDir ('d':'i':'r':' ':_) = True
    isDir _                   = False
    subfolders = map ((, Folder [] []) . drop 4) . filter isDir $ x
    files = map ((\[x, y] -> (y, read x)) . words) . filter (not . isDir) $ x

cd :: [String] -> String -> [String]
cd path ".." = take (length path - 1) path
cd path "/"  = []
cd path new  = path ++ [new]

insertAt :: [String] -> Folder -> Folder -> Folder
insertAt [] (Folder old_subfolders old_files) (Folder new_subfolders new_files) =
  Folder (new_subfolders ++ old_subfolders) (new_files ++ old_files)
  where
    subfolders =
      new_subfolders ++
      filter
        (\(name, content) -> name `elem` map fst new_subfolders)
        old_subfolders
    files =
      new_files ++
      filter (\(name, content) -> name `elem` map fst new_files) old_files
insertAt (folder:xs) (Folder subfolders files) new =
  Folder ((folder, inserted) : rest) files
  where
    subfolder =
      case lookup folder subfolders of
        (Just x) -> x
        Nothing  -> Folder [] []
    rest = filter (\(x, _) -> x /= folder) subfolders
    inserted = insertAt xs subfolder new

trim :: String -> String
trim (' ':xs) = trim xs
trim xs       = xs

commands :: String -> [[String]]
commands = filter (not . null) . map (map trim . lines) . splitOn "$"

size :: Folder -> Int
size (Folder subfolders files) = subfolder_size + file_size
  where
    subfolder_size = sum . map (size . snd) $ subfolders
    file_size = sum . map snd $ files

flatten :: Folder -> [Folder]
flatten f@(Folder subfolders _) =
  map snd subfolders ++ concatMap (flatten . snd) subfolders

part1 = sum . filter (< 100_000) . map size . flatten

part2 fs = minimum . filter (> to_delete) . map size . flatten $ fs
  where
    total = 70_000_000
    target = 30_000_000
    used = size fs
    free = total - used
    to_delete = target - free
