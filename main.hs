import           Data.Char
import           Data.Maybe            (mapMaybe, fromJust)
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
  --putStrLn $ unlines args
  let (actions, _ , _) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  -- TODO: Get actual progname in the options
  ls <- input opts >>= work opts
  output opts ls
  write opts ls
      where
        input :: Options -> IO TodoList
        input opt = bsReadFile (optTodoFile opt) >>= return . readTodoListFromString
        work :: Options -> TodoList -> IO TodoList
        work opt lst = case optWork opt of
          Add -> return $ add' (optInputStr opt) lst
          Remove -> return $ remove' (fromJust (optItemNumber opt)) lst
          Do -> return $ do' (fromJust (optItemNumber opt)) lst
          Undo -> return $ undo' (fromJust (optItemNumber opt)) lst
          Neither -> return lst
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

add' :: String -> TodoList -> TodoList
add' st ls = appendTodoItemToList ls (readTodoItemFromLine st)

remove' :: Int -> TodoList -> TodoList
remove' n ls = removeTodoItemFromList ls (n - 1)

do' :: Int -> TodoList -> TodoList
do' n ls = markTodoItemAsDone ls (n - 1)

undo' :: Int -> TodoList -> TodoList
undo' n ls = unMarkTodoItemAsDone ls (n - 1)

data Options = Options {
  optWork       :: WorkFlag,
  optItemNumber :: Maybe Int,
  optInputStr   :: String,
  optTodoFile   :: FilePath,
  optWriteOut   :: Bool,
  optStdOut     :: StdOutValue,
  optOutputStr  :: String,
  optProgName   :: String
  }

data WorkFlag = Add | Remove | Do | Undo | Neither

data StdOutValue = OutList | Version | OptString | None

defaultOptions :: Options
defaultOptions = Options {
                   optWork = Neither,
                   optItemNumber = Nothing,
                   optInputStr = "",
                   optTodoFile = "todo.txt",
                   optWriteOut = False,
                   optStdOut = None,
                   optOutputStr = "Empty",
                   optProgName = "htodotxt"
                 }

options :: [OptDescr (Options -> IO Options)]
options = [
 Option ['v'] ["version"] (NoArg optShowVersion) "Show version number",
 Option ['f'] ["file"] (ReqArg optSetFilePath "FILE") "Specify a todo file different from default",
 Option ['i'] ["item"] (ReqArg optSetItemNumber "ITEMNR") "Set the Todo item to work on",
 Option ['l'] ["list"] (NoArg optReadTodoList) "Show items in list with prepend number",
 Option ['a'] ["add"] (ReqArg optAddItem "TODOITEM") "Add a todo item to the list",
 Option ['r'] ["remove"] (NoArg optRemoveItem) "Remove a todo item from the list",
 Option ['d'] ["done"] (NoArg optDoneItem) "Mark todo item as done",
 Option ['u'] ["undone"] (NoArg optUnDoneItem) "Remove Mark as done",
 Option ['h'] ["help"] (NoArg optShowHelp) "Show this help"
 ]

showVersion :: String -> String
showVersion  progName = unlines ["hTodotxt - " ++ version, "Usage information: " ++ progName ++ " -h"]

optShowVersion :: Options -> IO Options
optShowVersion opt = return opt { optStdOut = Version }

optShowHelp :: Options -> IO Options
optShowHelp opt = return opt { optStdOut = OptString,
                            optOutputStr = helpstring header}
  where
    helpstring :: String -> String
    helpstring _ = usageInfo header options
    header :: String
    header = "Order of options matters, files come first."
  
optReadTodoList :: Options -> IO Options
optReadTodoList opt = return opt { optStdOut = OutList}

optSetItemNumber :: String -> Options -> IO Options
optSetItemNumber arg opt = return opt { optItemNumber = Just $ digitToInt $ head arg }

optSetFilePath :: String -> Options -> IO Options
optSetFilePath arg opt = return opt { optTodoFile = arg }

optAddItem :: String -> Options -> IO Options 
optAddItem arg opt = return opt { optWork = Add,
                                  optInputStr = arg,
                                  optWriteOut = True }

optRemoveItem :: Options -> IO Options
optRemoveItem opt = return opt { optWork = Remove,
                                 optWriteOut = True }

optDoneItem :: Options -> IO Options
optDoneItem opt = return opt { optWork = Do,
                               optWriteOut = True }

optUnDoneItem :: Options -> IO Options
optUnDoneItem opt = return opt { optWork = Undo,
                                 optWriteOut = True }
    
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
