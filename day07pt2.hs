import System.IO (readFile, print, putStrLn)
import System.Environment (getArgs)
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Debug.Trace (trace)

getFileLines :: String -> IO [String]
getFileLines f = do contents <- readFile f
                    return . lines $ contents

data FileSystemNode = Directory String Int [FileSystemNode] | File String Int deriving (Show, Eq)

instance Ord FileSystemNode where
         compare = comparing nodeSize

nodeSize :: FileSystemNode -> Int
nodeSize (File name size) = size
nodeSize (Directory name size children) = sum $ map nodeSize children

updateNodeSize :: FileSystemNode -> FileSystemNode
updateNodeSize (File name size) = File name size
updateNodeSize (Directory name size children) = Directory name (nodeSize (Directory name size children)) children

updateFileSystem :: (FileSystemNode -> FileSystemNode) -> FileSystemNode -> FileSystemNode
updateFileSystem f (File name size)               = File name size
updateFileSystem f (Directory name size children) = f $ Directory name size $ map (updateFileSystem f) children

filterFileSystem :: (FileSystemNode -> Bool) -> FileSystemNode -> [FileSystemNode]
filterFileSystem f (File name size)               = filter f [File name size]
filterFileSystem f (Directory name size children) = foldl (++) (filter f [Directory name size children]) $ map (filterFileSystem f) children 

newtype ChangeDirectory = ChangeDirectory String deriving (Show)

newtype List = List [FileSystemNode] deriving (Show)

type Command = Either ChangeDirectory List

appendList :: FileSystemNode -> (List, Int) -> (List, Int)
appendList node (List nodes, i) = (List (node:nodes), i+1)

readInt :: String -> Int
readInt str = read str :: Int

convList :: [String]  -> (List, Int)
convList (('$':x):xs) = (List [], 0)
convList []           = (List [], 0)
convList (x:xs)       = case splitOn " " x of
                            ["dir", dir]  -> appendList (Directory dir 0 []) $ convList xs
                            [size, file]  -> appendList (File file $ readInt size) $ convList xs
                            _             -> error "strange ls output"

convListCommand :: [String] -> [Command]
convListCommand strs = case convList strs of
                            (list, skip) -> Right list : convCommand (drop skip strs) 

convCommand :: [String] -> [Command]
convCommand (x:xs) = case take 4 x of
                          "$ cd" -> Left (ChangeDirectory $ drop 5 x) : convCommand xs
                          "$ ls" -> convListCommand xs
                          _      -> error "invalid command"
convCommand []     = []

data SplitDirectory = SplitDirectory String Int [FileSystemNode] [FileSystemNode] deriving (Show)

data Trail = Trail [SplitDirectory] FileSystemNode deriving (Show)

equalsNodeName :: String -> FileSystemNode -> Bool
equalsNodeName str (Directory name _ _) = str == name
equalsNodeName str (File name _)        = str == name

nodeName :: FileSystemNode -> String
nodeName (Directory name _ _) = name
nodeName (File name _)        = name

nodeNames :: [FileSystemNode] -> [String]
nodeNames = map nodeName

downDirectory :: Trail -> String -> Trail
downDirectory (Trail path (Directory nodeName size children)) child = case break (equalsNodeName child) children of
                                                                           (left, node:right) -> Trail (SplitDirectory nodeName size left right:path) node
                                                                           _                  -> error ("could not find child: " ++ child)
downDirectory (Trail _ file)                                  _     = error ("cannot go down non-directory: " ++ show file) 

upDirectory :: Trail -> Trail
upDirectory (Trail (SplitDirectory name size left right:path) node) = Trail path $ Directory name size $ left ++ node:right
upDirectory (Trail [] node)                                         = Trail [] node

root :: Trail
root = Trail [] (Directory "/" 0 [])

goRoot :: Trail -> Trail
goRoot (Trail splits (Directory "/" size children)) = Trail splits (Directory "/" size children)
goRoot trail                                        = goRoot $ upDirectory trail

changeDirectory :: Trail -> ChangeDirectory -> Trail
changeDirectory trail (ChangeDirectory "..") = upDirectory trail
changeDirectory trail (ChangeDirectory "/")  = goRoot trail 
changeDirectory trail (ChangeDirectory dir)  = downDirectory trail dir

insertNodes :: FileSystemNode -> [FileSystemNode] -> FileSystemNode
insertNodes (Directory dir size children) nodes = Directory dir size $ children ++ nodes
insertNodes file _                              = error ("cannot insert nodes into non-directory: " ++ show file)

insertNodesTrail :: Trail -> List -> Trail
insertNodesTrail (Trail path current) (List nodes) = Trail path $ insertNodes current nodes

buildFileSystem :: Trail -> [Command] -> Trail
buildFileSystem trail []                  = trail 
buildFileSystem trail (Left cd:commands)  = buildFileSystem (changeDirectory trail cd) commands
buildFileSystem trail (Right ls:commands) = buildFileSystem (insertNodesTrail trail ls) commands

calculateSizes :: Trail -> Trail
calculateSizes (Trail path (Directory "/" size children)) = Trail path $ updateFileSystem updateNodeSize $ Directory "/" size children 
calculateSizes trail                                      = calculateSizes $ goRoot trail

isDeletable :: Int -> FileSystemNode -> Bool
isDeletable systemSize (Directory _ size _) = size >= 30000000 - (70000000 - systemSize)
isDeletable _          _                    = False

findDeletable :: Trail -> [FileSystemNode]
findDeletable (Trail path (Directory "/" size children)) = filterFileSystem (isDeletable size) (Directory "/" size children)
findDeletable trail                                      = findDeletable $ goRoot trail

findMinimum :: [FileSystemNode] -> Int
findMinimum nodes = case minimum nodes of
                         File _ size        -> size
                         Directory _ size _ -> size

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content  <- getFileLines filename
                                print . findMinimum . findDeletable . calculateSizes . buildFileSystem root . convCommand $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
