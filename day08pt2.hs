import System.IO (readFile, print, putStrLn)
import System.Environment (getArgs)
import Data.List (findIndex)
import Debug.Trace (trace) 

getFileLines :: String -> IO [String]
getFileLines f = do contents <- readFile f
                    return . lines $ contents

readInt :: Char -> Int
readInt c = read [c] :: Int

type Tree = Int

type Location = (Int, Int)

type TreeLine = [Tree]

type Forest = [TreeLine]

type ForestPosition = (Forest, Location)

convRowInt :: String -> TreeLine
convRowInt = map readInt 

convRowsInt :: [String] -> Forest
convRowsInt = map convRowInt

initForestPosition :: [String] -> ForestPosition
initForestPosition lines = (convRowsInt lines, (0, 0))

smallerTrees :: Int -> TreeLine -> Int
smallerTrees height line = case findIndex (>= height) line of
                                Just count -> count + 1
                                Nothing    -> length line

visibleTrees :: TreeLine -> Int -> [Int]
visibleTrees line index = case splitAt index line of
                               (left, height:right) -> [smallerTrees height . reverse $ left, smallerTrees height right]
                               _                    -> error (show index ++ " is not a valid index")

horizontal :: Forest -> Int -> TreeLine
horizontal forest y = forest !! y

maxYCoordinate :: Forest -> Int
maxYCoordinate forest = length forest - 1

vertical :: Forest -> Int -> TreeLine
vertical lines x = map (!! x) lines

maxXCoordinate :: Forest -> Int
maxXCoordinate forest = length (head forest) - 1

treeScore :: ForestPosition -> Int
treeScore (forest, (x, y)) = product (visibleTrees (horizontal forest y) x ++ visibleTrees (vertical forest x) y)

printDebug :: (ForestPosition -> Bool) -> ForestPosition -> String
printDebug f (forest, location) = show location ++ ": " ++ show (f (forest, location))

scoreTrees :: (ForestPosition -> Int) -> ForestPosition -> [Int]
scoreTrees f (forest, (x, y)) | y == maxYCoordinate forest && x == maxXCoordinate forest = [f (forest, (x, y))]
                              | x < maxXCoordinate forest                                = f (forest, (x, y)) : scoreTrees f (forest, (x + 1, y))
                              | x == maxXCoordinate forest                               = f (forest, (x, y)) : scoreTrees f (forest, (0, y + 1))
                              | otherwise                                                = error "went out of bounds while counting trees"

maxScore :: ForestPosition -> Int
maxScore = foldl max 0 . scoreTrees treeScore

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content  <- getFileLines filename
                                print . maxScore $ initForestPosition content
               _          -> putStrLn "Usage: aoc2022 <filename>"
