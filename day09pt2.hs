import System.IO (readFile, print, putStrLn)
import System.Environment (getArgs)
import Debug.Trace (trace)
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

type RopeSegment = (Int, Int)

moveRopeSegment :: RopeSegment -> Direction -> RopeSegment
moveRopeSegment (x,y) UP    = (x,y+1)
moveRopeSegment (x,y) DOWN  = (x,y-1)
moveRopeSegment (x,y) LEFT  = (x-1,y)
moveRopeSegment (x,y) RIGHT = (x+1,y)

type Rope = [RopeSegment] 

initRope :: Rope
initRope = replicate 10 (0,0)

pullSegment :: RopeSegment -> RopeSegment -> (RopeSegment, RopeSegment)
pullSegment (headX,headY) (tailX,tailY)  | abs (headX - tailX) == 2 && abs (headY - tailY) == 2 = ((headX, headY),(tailX + quot (headX - tailX) 2, tailY + quot (headY - tailY) 2))
                                         | abs (headX - tailX) == 2 && abs (headY - tailY) == 1 = ((headX, headY),(tailX + quot (headX - tailX) 2, headY))
                                         | abs (headX - tailX) == 1 && abs (headY - tailY) == 2 = ((headX, headY),(headX, tailY + quot (headY - tailY) 2))
                                         | abs (headX - tailX) == 2                             = ((headX, headY),(tailX + quot (headX - tailX) 2, tailY))
                                         | abs (headY - tailY) == 2                             = ((headX, headY),(tailX, tailY + quot (headY - tailY) 2))
                                         | otherwise                                            = ((headX, headY),(tailX, tailY))

pullSegments :: Rope -> Rope
pullSegments []                  = []
pullSegments [tailSegment]       = [tailSegment]
pullSegments (first:second:rest) = case pullSegment first second of
                                        (newFirst, newSecond) -> newFirst:pullSegments (newSecond:rest)

tailPos :: Rope -> RopeSegment
tailPos = last

pullHeadSegment :: Rope -> Direction -> Rope
pullHeadSegment []          _         = []
pullHeadSegment (head:rope) direction = pullSegments (moveRopeSegment head direction:rope)

type VisitHistory = Set.Set RopeSegment 

initVisitHistory :: VisitHistory
initVisitHistory = Set.empty

markVisited :: VisitHistory -> RopeSegment -> VisitHistory
markVisited history pos = Set.insert pos history 

type RopeTrail = (Rope, VisitHistory)

initRopeTrail :: RopeTrail
initRopeTrail = (initRope, initVisitHistory)

moveRope :: RopeTrail -> Movement -> RopeTrail
moveRope (rope, history) (Movement direction 0)      = (rope, markVisited history $ tailPos rope) 
moveRope (rope, history) (Movement direction amount) = moveRope (pullHeadSegment rope direction, markVisited history $ tailPos rope) (Movement direction $ amount - 1)

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
