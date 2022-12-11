import System.IO (readFile, print, putStrLn)
import System.Environment (getArgs)
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

horizontal :: Forest -> Int -> TreeLine
horizontal forest y = forest !! y

maxYCoordinate :: Forest -> Int
maxYCoordinate forest = length forest - 1

foundLineOfSight :: TreeLine -> Int -> Bool
foundLineOfSight line index = case splitAt index line of
                                   (left, height:right) -> all (< height) left || all (< height) right
                                   _                    -> error ("index was out of bounds: " ++ show index)

visibleHorizontal :: ForestPosition -> Bool
visibleHorizontal (forest, (x, y)) = foundLineOfSight (horizontal forest y) x

vertical :: Forest -> Int -> TreeLine
vertical lines x = map (!! x) lines

maxXCoordinate :: Forest -> Int
maxXCoordinate forest = length (head forest) - 1

visibleVertical :: ForestPosition -> Bool
visibleVertical (forest, (x, y)) = foundLineOfSight (vertical forest x) y

isVisible :: ForestPosition -> Bool
isVisible position = visibleVertical position || visibleHorizontal position

printDebug :: (ForestPosition -> Bool) -> ForestPosition -> String
printDebug f (forest, location) = show location ++ ": " ++ show (f (forest, location))

countTrees :: (ForestPosition -> Bool) -> ForestPosition -> Int
countTrees f (forest, (x, y)) | y == maxYCoordinate forest && x == maxXCoordinate forest = fromEnum (f (forest, (x, y)))
                              | x < maxXCoordinate forest                                = fromEnum (f (forest, (x, y))) + countTrees f (forest, (x + 1, y))
                              | x == maxXCoordinate forest                               = fromEnum (f (forest, (x, y))) + countTrees f (forest, (0, y + 1))
                              | otherwise                                                = error "went out of bounds while counting trees"

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content  <- getFileLines filename
                                print . countTrees isVisible $ initForestPosition content
--print . initForestPosition $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
