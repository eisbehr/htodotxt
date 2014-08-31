import           Data.Char
import           Data.Maybe            (mapMaybe)
import           System.Console.GetOpt
import           System.Environment    (getArgs)
import           System.Exit()

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
  -- TODO: Get actual progname int the options
  ls <- input opts >>= work opts
  output opts ls
  write opts ls
      where
        input :: Options -> IO TodoList
        input opt = bsReadFile (optTodoFile opt) >>= return . readTodoListFromString
        work :: Options -> TodoList -> IO TodoList
        work opt = (optWork opt)
        output :: Options -> TodoList -> IO()
        output opt lst = case optStdOut opt of
            OptString -> putStrLn $ optOutputStr opt
            OutList -> putStrLn $ list lst
            Version -> putStrLn $ showVersion (optProgName opt)
            None -> putStr ""
        write :: Options -> TodoList -> IO()
        write opt lst 
            | optWriteOut opt = bsWriteFile (optTodoFile opt) $ writeTodoListToString lst
            | otherwise = putStr ""

data Options = Options {
  -- repl options
  optWork   :: TodoList -> IO TodoList,

  -- Value options
  optTodoFile   :: FilePath,
  optWriteOut   :: Bool,
  optStdOut     :: StdOutValue,
  optOutputStr  :: String,
  optProgName   :: String
  }

data StdOutValue = OutList | Version | OptString | None

defaultOptions :: Options
defaultOptions = Options {
                   optWork = return . id,

                   optTodoFile = "todo.txt",
                   optWriteOut = False,
                   optStdOut = Version,
                   optOutputStr = "Empty",
                   optProgName = "htodotxt"
                 }

options :: [OptDescr (Options -> IO Options)]
options = [
 Option ['v'] ["version"] (NoArg undefined) "Show version number",
 Option ['f'] ["file"] (ReqArg setFilePath "FILE") "Specify a todo file different from default",
 Option ['l'] ["list"] (NoArg readTodoList) "Show items in list with prepend number",
 Option ['a'] ["add"] (ReqArg addItem "TODOITEM") "Add a todo item to the list",
 Option ['r'] ["remove"] (ReqArg removeItem "ITEM NUMBER") "Remove a todo item from the list",
 Option ['d'] ["done"] (ReqArg doneItem "ITEM NUMBER") "Mark todo item as done",
 Option ['u'] ["undone"] (ReqArg unDoneItem "ITEM NUMBER") "Remove Mark as done",
 Option ['h'] ["help"] (NoArg showHelp) "Show this help"
 ]

showVersion :: String -> String
showVersion  progName = unlines ["hTodotxt - " ++ version, "Usage information: " ++ progName ++ " -h"]

showHelp :: Options -> IO Options
showHelp opt = return opt { optStdOut = OptString,
                            optOutputStr = helpstring header}
  where
    helpstring :: String -> String
    helpstring _ = usageInfo header options
    header :: String
    header = "Order of options matters, files come first."
  
readTodoList :: Options -> IO Options
readTodoList opt = return opt { optStdOut = OutList}

setFilePath :: String -> Options -> IO Options
setFilePath arg opt = return opt { optTodoFile = arg }

addItem :: String -> Options -> IO Options 
addItem arg opt = return opt { optWork = return . appendIt arg,
                               optWriteOut = True,
                               optStdOut = None }
    where
      appendIt :: String -> TodoList -> TodoList
      appendIt st ls = appendTodoItemToList ls (readTodoItemFromLine st)

removeItem :: String -> Options -> IO Options
removeItem arg opt = return opt { optWork = return . remove (digitToInt (head arg)),
                                  optWriteOut = True,
                                  optStdOut = None }
  where
    remove :: Int -> TodoList -> TodoList
    remove n ls = removeTodoItemFromList ls (n - 1)

doneItem :: String -> Options -> IO Options
doneItem arg opt = return opt { optWork = return . done' (digitToInt (head arg)),
                                optWriteOut = True,
                                optStdOut = None }
  where
    done' :: Int -> TodoList -> TodoList
    done' n ls = markTodoItemAsDone ls (n - 1)

unDoneItem :: String -> Options -> IO Options
unDoneItem arg opt = return opt { optWork = return . unDone (digitToInt (head arg)),
                                  optWriteOut = True,
                                  optStdOut = None }
  where
    unDone :: Int -> TodoList -> TodoList
    unDone n ls = unMarkTodoItemAsDone ls (n - 1)

--appendLine :: String -> String -> IO String
--appendLine line lst = return .  writeTodoListToString $ appendTodoItemToList (readTodoListFromString lst) (readTodoItemFromLine line)

list :: TodoList -> String
list tl = unlines $ prepareTodoListForPrint [] tl

-- ### Pure todotxt functions ### --
data TodoItem = TodoItem { text     :: String
                         , priority :: Maybe String
                         , projects :: [String]
                         , contexts :: [String]
                         , done     :: Bool -- todo - add creation date
                         } deriving (Show)      -- todo - add done 'x'

type TodoList = [TodoItem]

prepareTodoListForPrint :: [String] -> TodoList -> [String]
prepareTodoListForPrint acc lst
    | not (null lst) = prepareTodoListForPrint (text (last lst):acc) (init lst)
    | otherwise = zipWith (\a b -> show a ++ ": " ++ b) ([1..] :: [Int]) acc

readTodoListFromString :: String -> TodoList
readTodoListFromString s = map readTodoItemFromLine $ lines s

todoItemAsString :: (String -> String) -> TodoItem -> TodoItem
todoItemAsString f it = readTodoItemFromLine $ f (text it)

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
         , contexts = ctxt
         , done     = dne }
  where 
    ws = words line
    prio = todoPriorityFromWord . head $ ws
    proj = mapMaybe todoProjectFromWord ws
    ctxt = mapMaybe todoContextFromWord ws
    dne  = head ws == "x"
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

overwriteTodoItemInList :: TodoList -> Int -> TodoItem -> TodoList
overwriteTodoItemInList ls n = insertTodoItemIntoList (removeTodoItemFromList ls n) n

appendTodoItemToList :: TodoList -> TodoItem -> TodoList
appendTodoItemToList tl ti =  tl ++ [ti]

removeTodoItemFromList :: TodoList -> Int -> TodoList
removeTodoItemFromList ls n
  | n > length ls || n < 0 = ls
  | n == length ls = init ls
  | n == 0 = tail ls
  | otherwise = ys ++ tail zs
  where (ys, zs) = splitAt n ls

markTodoItemAsDone :: TodoList -> Int -> TodoList
markTodoItemAsDone ls n = case getTodoItemFromList ls n of
  Just it -> overwriteTodoItemInList ls n (markDone it)
  Nothing -> ls
  where
    markDone it = if not $ done it
                  then ("x " ++) `todoItemAsString` it
                  else it

unMarkTodoItemAsDone :: TodoList -> Int -> TodoList
unMarkTodoItemAsDone ls n = case getTodoItemFromList ls n of
  Just it -> overwriteTodoItemInList ls n (unMarkDone it)
  Nothing -> ls
  where
    unMarkDone it = if done it
                    then (unwords . tail . words) `todoItemAsString` it
                    else it

getTodoItemFromList :: TodoList -> Int -> Maybe TodoItem
getTodoItemFromList ls n
  | n > length ls || n < 0 = Nothing
  | n == length ls = Just $ last ls
  | n == 0 = Just $ head ls
  | otherwise = Just $ head zs
  where (_, zs) = splitAt n ls
