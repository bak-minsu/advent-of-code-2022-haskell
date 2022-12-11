import System.IO (readFile, print, putStrLn)
import System.Environment (getArgs)
import Debug.Trace (trace)
import Data.Function ((&))
import qualified Data.Set as Set

data Direction = LEFT | RIGHT | DOWN | UP deriving (Show, Enum)

toDirection :: Char -> Direction
toDirection 'L' = LEFT
toDirection 'R' = RIGHT
toDirection 'D' = DOWN
toDirection 'U' = UP
toDirection c   = error (c : " is not a valid direction")

data Movement = Movement Direction Int deriving (Show)

toMovement :: String -> Movement
toMovement (direction:' ':count) = Movement (toDirection direction) (read count :: Int)
toMovement str                   = error (str ++ " is not a valid movement")

toMovements :: [String] -> [Movement]
toMovements = map toMovement

type Position = (Int, Int)

movePosition :: Position -> Direction -> Position
movePosition (x,y) UP    = (x,y+1)
movePosition (x,y) DOWN  = (x,y-1)
movePosition (x,y) LEFT  = (x-1,y)
movePosition (x,y) RIGHT = (x+1,y)

data Rope = Rope Position Position deriving (Show)

initRope :: Rope
initRope = Rope (0,0) (0,0)

pullTail :: Rope -> Rope
pullTail (Rope (headX, headY) (tailX, tailY)) | abs (headX - tailX) == 2 && abs (headY - tailY) == 1 = Rope (headX, headY) (tailX + quot (headX - tailX) 2, headY)
                                              | abs (headX - tailX) == 1 && abs (headY - tailY) == 2 = Rope (headX, headY) (headX, tailY + quot (headY - tailY) 2)
                                              | abs (headX - tailX) == 2                             = Rope (headX, headY) (tailX + quot (headX - tailX) 2, tailY)
                                              | abs (headY - tailY) == 2                             = Rope (headX, headY) (tailX, tailY + quot (headY - tailY) 2)
                                              | otherwise                                            = Rope (headX, headY) (tailX, tailY)

type VisitHistory = Set.Set Position 

initVisitHistory :: VisitHistory
initVisitHistory = Set.empty

markVisited :: VisitHistory -> Position -> VisitHistory
markVisited history pos = Set.insert pos history 

type RopeTrail = (Rope, VisitHistory)

initRopeTrail :: RopeTrail
initRopeTrail = (initRope, initVisitHistory)

moveRope :: RopeTrail -> Movement -> RopeTrail
moveRope (Rope headPos tailPos, history) (Movement direction 0)      = (Rope headPos tailPos, markVisited history tailPos) 
moveRope (Rope headPos tailPos, history) (Movement direction amount) = moveRope (Rope (movePosition headPos direction) tailPos & pullTail, markVisited history tailPos) (Movement direction $ amount-1)

moveRopeMany :: RopeTrail -> [Movement] -> RopeTrail
moveRopeMany = foldl moveRope

countVisited :: RopeTrail -> Int
countVisited (_, history) = length history

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content <- lines <$> readFile filename
                                print . countVisited . moveRopeMany initRopeTrail $ toMovements content
               _          -> putStrLn "Usage: aoc2022 <filename>"
