-- Learn you a haskell task list command line app

-- TODO: figure out how to use absolute paths to open files
import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           , ("bump", bump)
           , ("help", help)
           , ("new" , new)]

help :: [String] -> IO ()
help ["add"] = do
  putStrLn "usage: add [filename] [item]"
help ["view"] = do
  putStrLn "usage: view [filename]"
help ["remove"] = do
  putStrLn "usage: remove [filename] [item number]"
help ["bump"] = do
  putStrLn "usage: bump [filename] [item number]"
help ["new"] = do
  putStrLn "usage: new [filename] [item 1] (item 2) ... (item n)"
help _ = do
  putStrLn "Available commands are:"
  let commands = unlines $ map ('\t':) $ map fst dispatch
  putStr commands
  putStrLn "Type 'help [command]' for more info"
-- TODO: add more detailed help info for each command using pattern mathcing

new :: [String] -> IO ()
new (filename:items) = do
  hndl <- openFile  filename WriteMode

  hPutStr hndl (unlines items)
  hClose hndl


bump :: [String] -> IO () --bumps an item to the top of the list
bump [fileName, numberString] = do
  contents <- readFile fileName
  (tmpName, tmpHndl) <- openTempFile "." "temp"
  let number = read numberString
      oldTodo = lines contents
      newTodo = (oldTodo !! number) : delete (oldTodo !! number) oldTodo
  hPutStr tmpHndl $ unlines newTodo
  hClose tmpHndl
  removeFile fileName
  renameFile tmpName fileName

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let (description:todoTasks) = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  contents <- readFile fileName
  (tmpName, tHndl) <- openTempFile "~/.tmLists" "temp"
  let number = read numberString
      todoTasks = lines contents
      newTodo = delete (todoTasks !! number) todoTasks
  hPutStr tHndl $ unlines newTodo
  hClose tHndl
  removeFile fileName
  renameFile tmpName fileName
           
main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args

