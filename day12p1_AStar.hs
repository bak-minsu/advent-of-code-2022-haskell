import System.IO (readFile, print, putStrLn)
import System.Environment (getArgs)
import Debug.Trace (trace) 
import qualified Data.Heap as Heap
import qualified Data.Map as Map

type Height = Int

type HeightMap = [[Height]]

charToHeight :: Char -> Height
charToHeight 'S' = 0
charToHeight 'E' = 25
charToHeight c   = fromEnum c - 97

toMapHorizontal :: String -> [Height]
toMapHorizontal = map charToHeight

toHeightMap :: [String] -> HeightMap
toHeightMap = map toMapHorizontal

maxXMap :: HeightMap -> Int
maxXMap m = (minimum . map length $ m) - 1

maxYMap :: HeightMap -> Int
maxYMap m = length m - 1

data Direction = UP | DOWN | LEFT | RIGHT deriving (Show, Enum)

positionAtDirection :: Position -> Direction -> Position
positionAtDirection (x,y) UP    = (x  , y-1)
positionAtDirection (x,y) DOWN  = (x  , y+1)
positionAtDirection (x,y) LEFT  = (x-1, y  )
positionAtDirection (x,y) RIGHT = (x+1, y  )

type Position = (Int, Int)

mapHeightAt :: HeightMap -> Position -> Height
mapHeightAt heightMap (x,y) = (heightMap !! y) !! x

isOutOfBounds :: HeightMap -> Position -> Bool
isOutOfBounds heightMap (x,y) = x < 0 || x > maxXMap heightMap || y < 0 || y > maxYMap heightMap

moveHeight :: HeightMap -> Position -> Position -> Int
moveHeight heightMap from to = mapHeightAt heightMap to - mapHeightAt heightMap from

isPossibleMove :: HeightMap -> Position -> Position -> Bool
isPossibleMove heightMap from to | isOutOfBounds heightMap to       = False
                                 | moveHeight heightMap from to > 1 = False
                                 | otherwise                        = True 

possibleMoves :: HeightMap -> Position -> [Position]
possibleMoves heightMap fromPos = filter (isPossibleMove heightMap fromPos) $ map (positionAtDirection fromPos) [UP, DOWN, LEFT, RIGHT]

data GPS = GPS {
    startPosition :: Position,
    endPosition   :: Position,
    gpsMap        :: HeightMap
} deriving (Show)

charPosition :: ([String], Position) -> Char -> Position
charPosition ((current:xs):ys, (x, y)) c | c == current = (x,y)
                                         | otherwise    = charPosition (xs:ys, (x+1, y)) c
charPosition ([]:ys, (x,y))            c = charPosition (ys, (0, y+1)) c 
charPosition ([], _)                   c = error ("cannot find char: " ++ [c]) 

findStart :: [String] -> Position
findStart content = charPosition (content, (0,0)) 'S'

findEnd :: [String] -> Position
findEnd content = charPosition (content, (0,0)) 'E'

toGPS :: [String] -> GPS
toGPS content = trace (show (findStart content) ++ show (findEnd content)) GPS (findStart content) (findEnd content) (toHeightMap content)

data Node = Node {
    totalDistance :: Int,
    nodeParent    :: Node,
    nodePosition  :: Position
} | NilNode deriving (Show, Eq)

instance Ord Node where
    compare (Node totalLeft _ _) (Node totalRight _ _) = compare totalLeft totalRight
    compare NilNode _                                  = LT 
    compare _ NilNode                                  = GT 

initNode :: GPS -> Node
initNode gps = Node (maxBound :: Int) NilNode (startPosition gps)

rebuildPath :: Node -> [Position]
rebuildPath NilNode = []
rebuildPath node    = nodePosition node:rebuildPath (nodeParent node)

distanceToStart :: Node -> Int
distanceToStart NilNode = 0
distanceToStart node    = 1 + distanceToStart (nodeParent node)

positionEquals :: Node -> Node -> Bool
positionEquals nodeLeft nodeRight = nodePosition nodeLeft == nodePosition nodeRight

manhattanDistance :: Position -> Position -> Int
manhattanDistance (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

heightDifference :: HeightMap -> Position -> Position -> Int
heightDifference heightMap pos1 pos2 = abs (mapHeightAt heightMap pos1 - mapHeightAt heightMap pos2)

distanceToEnd :: GPS -> Node -> Int
distanceToEnd gps node = manhattanDistance (endPosition gps) (nodePosition node) + heightDifference (gpsMap gps) (endPosition gps) (nodePosition node)
--distanceToEnd gps node = manhattanDistance (endPosition gps) (nodePosition node)

successor :: Node -> Position -> Position -> Node
successor parent endPos position = Node (manhattanDistance endPos position + distanceToStart parent + 1) parent position

possibleSuccessors :: GPS -> Node -> [Node]
possibleSuccessors gps node = map (successor node $ endPosition gps) $ possibleMoves (gpsMap gps) $ nodePosition node

type Open = Heap.MinHeap Node

printOpen :: Open -> String
printOpen open = "open - Distances: " ++ show (map totalDistance $ Heap.toList open) ++ " Positions: " ++ show (map nodePosition $ Heap.toList open)

initOpen :: GPS -> Node -> Open
initOpen gps = Heap.singleton

nextOpen :: Open -> (Node, Open)
nextOpen open = case Heap.view open of
                     Nothing           -> error "open was empty"
                     Just (node, open) -> (node, open)

removeSuccessors :: Open -> [Node] -> Open
removeSuccessors open []           = open
removeSuccessors open (node:nodes) = Heap.filter (not . positionEquals node) open `removeSuccessors` nodes

insertSuccessors :: Open -> [Node] -> Open
insertSuccessors open nodes = foldr Heap.insert (removeSuccessors open nodes) nodes

hasLongerStartDistance :: Node -> Node -> Bool
hasLongerStartDistance node found = distanceToStart node >= distanceToStart found

foundBetterStartDistance :: Open -> Node -> Bool
foundBetterStartDistance open successor | foundCount == 0 = False
                                        | foundCount == 1 = True
                                        | otherwise       = error "found more than one instance of a node in open list"
                                        where foundCount = length $ filter (hasLongerStartDistance successor) $ Heap.takeWhile (positionEquals successor) open

type Close = Map.Map Position Node 

printClose :: Close -> String
printClose close = "close - " ++ show (map (nodePosition . snd) $ Map.toList close)

insertNode :: Node -> Close -> Close
insertNode node = Map.insert (nodePosition node) node

type Path = [Position]

data PathFinderState = PathFinderState {
    stateGPS   :: GPS,
    stateOpen  :: Open, 
    stateClose :: Close, 
    stateNode  :: Node
} deriving (Show)

initPathFinderState :: [String] -> PathFinderState
initPathFinderState content = let gps = toGPS content
                              in PathFinderState gps (initOpen gps $ initNode gps) Map.empty $ initNode gps

keepSuccessor :: Open -> Close -> Node -> Bool
keepSuccessor open close node | Map.member (nodePosition node) close = False
                              | foundBetterStartDistance open node   = False
                              | otherwise                            = True

successorsToProcess :: PathFinderState -> [Node]
successorsToProcess (PathFinderState gps open close node) = filter (keepSuccessor open close) $ possibleSuccessors gps node

nextState :: PathFinderState -> PathFinderState
nextState (PathFinderState gps open close node) = case nextOpen open of
                                                       (next, newOpen) -> PathFinderState gps (insertSuccessors newOpen $ successorsToProcess $ PathFinderState gps newOpen close next) (insertNode next close) next

aStar :: PathFinderState -> Path
aStar state | nodePosition (stateNode state) == endPosition (stateGPS state) = rebuildPath $ stateNode state 
            | not . Heap.isEmpty $ stateOpen state                           = aStar $ nextState state
            | otherwise                                                      = error ("could not reach end" ++ show (stateGPS state))

pathDistance :: Path -> Int
pathDistance path = trace (show path) length path - 1

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content <- lines <$> readFile filename
                                print . pathDistance . aStar . initPathFinderState $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
