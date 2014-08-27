import           Data.Char
import           Data.Maybe            (mapMaybe)
import           System.Console.GetOpt
import           System.Environment    (getArgs, getProgName)
import           System.Exit

import qualified Data.ByteString.Char8 as C


-- ### Global stuff ### --
version :: String
version = "v0.1"

-- ### IO ### --

-- helpers for non-lazy IO

bsReadFile :: FilePath -> IO String
bsReadFile path = C.readFile path >>= return . C.unpack

bsWriteFile :: FilePath -> String -> IO()
bsWriteFile path s = C.writeFile path $ C.pack s

main :: IO()
main = do
  args <- getArgs
  let (actions, _ , _) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optInput = input,
                optWork = work,                
                optOutput = output } = opts
  input >>= work >>= output

data Options = Options {
      optInput  :: IO String,
      optWork   :: String -> IO String,
      optOutput :: String -> IO()
    }

defaultOptions :: Options
defaultOptions = Options {
                   optInput = bsReadFile "todo.txt",
                   optWork = showVersion,
                   optOutput = putStrLn
                 }

options :: [OptDescr (Options -> IO Options)]
options = [
 Option ['v'] ["version"] (NoArg undefined) "show version number",
 Option ['l'] ["list"] (NoArg readTodoList) "show items in list with prepend number",
 Option ['f'] ["file"] (ReqArg setFilePath "FILE") "specify a todo file different from default",
 Option ['a'] ["add"] (ReqArg addItem "TODOITEM") "add a todo item to the list",
 Option ['r'] ["remove"] (ReqArg removeItem "ITEM NUMBER") "remove a todo item from the list"
 ]

showVersion :: String -> IO String
showVersion _ = do  
  progName <- getProgName
  return $ unlines ["hTodotxt - " ++ version, "Usage information: " ++ progName ++ " -h"]
  
readTodoList :: Options -> IO Options
readTodoList opt = return opt { optInput = bsReadFile "todo.txt",
                                optWork = list . readTodoListFromString }

setFilePath :: String -> Options -> IO Options
setFilePath arg opt = return opt { optInput = bsReadFile arg,
                                   optOutput = bsWriteFile arg }

addItem :: String -> Options -> IO Options 
addItem arg opt = return opt { optWork = appendLine arg,
                               optOutput = bsWriteFile "todo.txt" }

removeItem :: String -> Options -> IO Options
removeItem arg opt = return opt { optWork = removeItem (digitToInt (head arg)),
                                  optOutput = bsWriteFile "todo.txt" }
  where
    removeItem :: Int -> String -> IO String
    removeItem n ls = return . writeTodoListToString $ removeTodoItemFromList (readTodoListFromString ls) (n - 1)

appendLine :: String -> String -> IO String
appendLine line list = return $ unlines ((lines list) ++ [line])

list :: TodoList -> IO String
list tl = return (unlines $ prepareTodoListForPrint [] tl)

-- ### Pure todotxt functions ### --
data TodoItem = TodoItem { text     :: String
                         , priority :: Maybe String
                         , projects :: [String]
                         , contexts :: [String] -- todo - add creation date
                         } deriving (Show)      -- todo - add done 'x'

type TodoList = [TodoItem]

prepareTodoListForPrint :: [String] -> TodoList -> [String]
prepareTodoListForPrint acc lst
    | not (null lst) = prepareTodoListForPrint (text (last lst):acc) (init lst)
    | otherwise = zipWith (\a b -> show a ++ ": " ++ b) ([1..] :: [Int]) acc

readTodoListFromString :: String -> TodoList
readTodoListFromString s = map readTodoItemFromLine $ lines s

writeTodoListToString :: TodoList -> String
writeTodoListToString li = unlines $ makeString [] li
  where
    makeString :: [String] -> TodoList -> [String]
    makeString acc lst
      | null lst = reverse acc
      | otherwise = makeString (text (head lst) : acc) (tail lst)

readTodoItemFromLine :: String -> TodoItem
readTodoItemFromLine line =
    TodoItem { text = line
         , priority = prio
         , projects = proj
         , contexts = ctxt }
  where 
    ws = words line
    prio = todoPriorityFromWord . head $ ws
    proj = mapMaybe todoProjectFromWord ws
    ctxt = mapMaybe todoContextFromWord ws
    todoPriorityFromWord :: String -> Maybe String
    todoPriorityFromWord xs = 
        if head xs == '('
           && last xs == ')'
           && length xs == 3
           && isUpper (xs !! 1)
           && elem (xs !! 1) ['A'..'Z']
        then Just [ x | x <- xs, x /='(' && x /= ')']
        else Nothing
    todoProjectFromWord :: String -> Maybe String
    todoProjectFromWord xs = 
        if head xs == '+'
        then Just $ tail xs
        else Nothing
    todoContextFromWord :: String -> Maybe String
    todoContextFromWord xs = 
        if head xs == '@'
        then Just $ tail xs
        else Nothing

insertTodoItemIntoList :: TodoList -> Int -> TodoItem -> TodoList
insertTodoItemIntoList ls n it = ys ++ [it] ++ zs
  where (ys, zs) = splitAt n ls

appendTodoItemToList :: TodoList -> TodoItem -> TodoList
appendTodoItemToList tl ti =  tl ++ [ti]

removeTodoItemFromList :: TodoList -> Int -> TodoList
removeTodoItemFromList ls n
  | n > length ls || n < 0 = ls
  | n == length ls = init ls
  | n == 0 = tail ls
  | otherwise = ys ++ (tail zs)
  where (ys, zs) = splitAt n ls
