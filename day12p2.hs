import System.IO (readFile, print, putStrLn)
import System.Environment (getArgs)
import Debug.Trace (trace) 
import qualified Data.Heap as Heap
import qualified Data.Map as Map

data Height = Height {
    heightChar :: Char,
    heightInt  :: Int
} deriving (Show)

type HeightMap = [[Height]]

charToHeight :: Char -> Height
charToHeight 'S' = Height 'a' 0
charToHeight 'E' = Height 'E' 25
charToHeight c   = Height c (fromEnum c - 97)

toMapHorizontal :: String -> [Height]
toMapHorizontal = map charToHeight

toHeightMap :: [String] -> HeightMap
toHeightMap = map toMapHorizontal

maxXMap :: HeightMap -> Int
maxXMap m = (minimum . map length $ m) - 1

maxYMap :: HeightMap -> Int
maxYMap m = length m - 1

data Direction = UP | DOWN | LEFT | RIGHT deriving (Show, Enum)

data Position = Position Int Int | NilPosition deriving (Show, Eq, Ord)

positionAtDirection :: Position -> Direction -> Position
positionAtDirection NilPosition    _     = error "cannot generate direction for nil position"
positionAtDirection (Position x y) UP    = Position x     (y-1)
positionAtDirection (Position x y) DOWN  = Position x     (y+1)
positionAtDirection (Position x y) LEFT  = Position (x-1) y
positionAtDirection (Position x y) RIGHT = Position (x+1) y

isOutOfBounds :: HeightMap -> Position -> Bool
isOutOfBounds _         NilPosition    = error "cannot check out of bounds of nil position"
isOutOfBounds heightMap (Position x y) = x < 0 || x > maxXMap heightMap || y < 0 || y > maxYMap heightMap

mapHeightAt :: HeightMap -> Position -> Int
mapHeightAt _         NilPosition    = error "cannot get height at nil position"
mapHeightAt heightMap (Position x y) | isOutOfBounds heightMap (Position x y) = error "cannot get height outside of bounds"
                                     | otherwise                              = heightInt $ (heightMap !! y) !! x

moveHeight :: HeightMap -> Position -> Position -> Int
moveHeight heightMap from to = mapHeightAt heightMap from - mapHeightAt heightMap to

isPossibleMove :: HeightMap -> Position -> Position -> Bool
isPossibleMove heightMap from to | isOutOfBounds heightMap to       = False
                                 | moveHeight heightMap from to > 1 = False
                                 | otherwise                        = True 

possibleMoves :: HeightMap -> Position -> [Position]
possibleMoves heightMap fromPos = filter (isPossibleMove heightMap fromPos) $ map (positionAtDirection fromPos) [UP, DOWN, LEFT, RIGHT]

data GPS = GPS {
    endPosition    :: Position,
    gpsMap         :: HeightMap
} deriving (Show)

charPosition :: [String] -> Position -> Char -> Position
charPosition _                 NilPosition    c                = error "cannot find char at position NilPosition"
charPosition []                _              c                = error "char was not found"
charPosition ([]:ys)           (Position x y) c                = charPosition ys (Position 0 (y+1)) c
charPosition ((current:xs):ys) (Position x y) c | c == current = Position x y
                                                | otherwise    = charPosition (xs:ys) (Position (x+1) y) c

findCharPosition :: [String] -> Char -> Position
findCharPosition content = charPosition content (Position 0 0)

findEnd :: [String] -> Position
findEnd content = findCharPosition content 'E'

toGPS :: [String] -> GPS
toGPS content = GPS (findEnd content) (toHeightMap content)

data Node = Node {
    distanceToStart :: Int,
    nodeParent      :: Position,
    nodePosition    :: Position
} deriving (Show, Eq)

instance Ord Node where
    compare nodeLeft nodeRight = compare (distanceToStart nodeLeft) (distanceToStart nodeRight)

initStartNode :: Position -> Node
initStartNode = Node 0 NilPosition


positionEquals :: Node -> Node -> Bool
positionEquals nodeLeft nodeRight = nodePosition nodeLeft == nodePosition nodeRight

heightDifference :: HeightMap -> Position -> Position -> Int
heightDifference heightMap pos1 pos2 = abs (mapHeightAt heightMap pos1 - mapHeightAt heightMap pos2)

nodeHeightIsZero :: HeightMap -> Node -> Bool
nodeHeightIsZero heightMap node = mapHeightAt heightMap (nodePosition node) == 0

type Open = Heap.MinHeap Node

initOpen :: Position -> Open
initOpen startPos = Heap.singleton $ initStartNode startPos

popOpen :: Open -> (Node, Open)
popOpen open = case Heap.view open of
                    Nothing           -> error "nothing to pop"
                    Just (node, open) -> (node, open)

removeNodesOpen :: Open -> [Node] -> Open
removeNodesOpen open nodes = Heap.filter (not . flip elem nodes) open

insertOpen :: Open -> [Node] -> Open
insertOpen open nodes = foldl (flip Heap.insert) (removeNodesOpen open nodes) nodes 

type Closed = Map.Map Position Node

possibleSuccessors :: HeightMap -> Node -> [Node]
possibleSuccessors heightMap parent = map (Node (distanceToStart parent + 1) (nodePosition parent)) $ possibleMoves heightMap $ nodePosition parent

initClosed :: Closed
initClosed = Map.empty 

insertClosed :: Closed -> Node -> Closed
insertClosed closed node = Map.insert (nodePosition node) node closed

notExistClosed :: Closed -> Node -> Bool
notExistClosed closed = flip Map.notMember closed . nodePosition

nodeAtPosition :: Closed -> Position -> Node
nodeAtPosition closed position = case Map.lookup position closed of
                                      Nothing   -> error ("could not find node at " ++ show position)
                                      Just node -> node

sameNodeShorter :: Closed -> Node -> Bool
sameNodeShorter closed node | notExistClosed closed node                                                         = True
                            | distanceToStart node < distanceToStart (nodeAtPosition closed $ nodePosition node) = True
                            | otherwise                                                                          = False

keepLessThan :: Closed -> [Node] -> [Node]
keepLessThan closed = filter (sameNodeShorter closed) 

closedZeroHeights :: Closed -> HeightMap -> [Node]
closedZeroHeights closed heightMap = Map.elems $ Map.filter (nodeHeightIsZero heightMap) closed

data DijkstraState = DijkstraState {
    stateOpen   :: Open,
    stateClosed :: Closed,
    stateGPS    :: GPS
} deriving (Show)

initState :: [String] -> DijkstraState
initState strs = let gps = toGPS strs
                 in DijkstraState (initOpen $ endPosition gps) initClosed gps 

nextState :: (Node, Open) -> DijkstraState -> DijkstraState
nextState (next, open) (DijkstraState _ closed gps) = DijkstraState (insertOpen open $ keepLessThan closed $ possibleSuccessors (gpsMap gps) next) (insertClosed closed next) gps

fillClosed :: DijkstraState -> DijkstraState
fillClosed state | Heap.isEmpty $ stateOpen state = state
                 | otherwise                      = fillClosed $ nextState (popOpen $ stateOpen state) state

zeroHeightNodes :: DijkstraState -> [Node]
zeroHeightNodes state = closedZeroHeights (stateClosed state) (gpsMap $ stateGPS state)

shortestPathSize :: DijkstraState -> Int
shortestPathSize state = minimum $ map distanceToStart $ zeroHeightNodes $ fillClosed state

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content <- lines <$> readFile filename
                                print . shortestPathSize . initState $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
