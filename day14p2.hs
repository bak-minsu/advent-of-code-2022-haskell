import System.IO (readFile, print, putStrLn)
import System.Environment (getArgs)
import Debug.Trace (trace) 
import Data.List.Split (splitOn)
import Data.Maybe (isNothing)

type Position = (Int, Int)

type Formation = [Position]

toPosition :: String -> Position
toPosition str = case break (== ',') str of
                      (left, comma:right) -> (read left :: Int, read right :: Int)
                      _                   -> error ("incorrect position format: " ++ str)

toFormation :: String -> Formation
toFormation str = case splitOn " -> " str of
                       splitList -> map toPosition splitList

type Formations = [Formation]

toFormations :: [String] -> Formations 
toFormations = map toFormation

maxXFormations :: Formations -> Int
maxXFormations formations = 2 * maximum (map fst $ concat formations)

maxYFormations :: Formations -> Int
maxYFormations formations = 2 + maximum (map snd $ concat formations)

appendFloor :: Formations -> Formations
appendFloor formations = let maxY = maxYFormations formations 
                         in [(0, maxY), (maxXFormations formations, maxY)]:formations

data Material = AIR | ROCK | SAND deriving (Show, Enum, Eq)

materialAscii :: Material -> Char
materialAscii AIR  = '.'
materialAscii ROCK = '#'
materialAscii SAND = 'o'

data CaveScan = CaveScan {
    scanMap  :: [[Material]],
    maxXScan :: Int,
    maxYScan :: Int
}

instance Show CaveScan where
    show scan = unlines $ map (map materialAscii) $ scanMap scan

materialAtPosition :: CaveScan -> Position -> Material
materialAtPosition scan (x,y) = scanMap scan !! y !! x

isOutOfBounds :: CaveScan -> Position -> Bool
isOutOfBounds scan (x, y) = x > maxXScan scan || y > maxYScan scan

type Vector = (Int, Int)

pixelVector :: Position -> Position -> Vector
pixelVector (startX, startY) (endX, endY) | startX == endX && startY == endY = (0, 0)
                                          | startX == endX                   = (0, div (endY - startY) $ abs (endY -startY))
                                          | startY == endY                   = (div (endX - startX) $ abs (endX - startX), 0)
                                          | otherwise                        = (div (endX - startX) $ abs (endX - startX), div (endY - startY) $ abs (endY - startY))

drawPoint :: CaveScan -> Position -> Material -> CaveScan
drawPoint (CaveScan scan maxX maxY) (x,y) material = CaveScan (take y scan ++ (take x (scan!!y) ++ material:drop (x+1) (scan!!y)):drop (y+1) scan) maxX maxY

moveVector :: Position -> Vector -> Position
moveVector (x, y) (pixelVectorX, pixelVectorY) = (x + pixelVectorX, y + pixelVectorY)

drawLine :: CaveScan -> (Position, Position) -> CaveScan
drawLine scan (start, end) | start == end = drawPoint scan start ROCK
                           | otherwise    = drawPoint scan start ROCK `drawLine` (moveVector start $ pixelVector start end, end)

drawFormation :: CaveScan -> Formation -> CaveScan
drawFormation scan []                    = scan
drawFormation scan [_]                   = scan
drawFormation scan (start:end:formation) = drawLine scan (start, end) `drawFormation` (end:formation)

drawFormations :: CaveScan -> Formations -> CaveScan
drawFormations = foldl drawFormation 

initCaveScan :: Formations -> CaveScan
initCaveScan formations = let maxX     = maxXFormations formations
                              maxY     = maxYFormations formations
                              initScan = replicate (1 + maxY) $ replicate (1 + maxX) AIR
                          in drawFormations (CaveScan initScan maxX maxY) formations

sandInitPosition :: Position
sandInitPosition = (500, 0)

data MovePossible = YES | NO | OUTOFBOUNDS deriving (Eq, Show, Enum)

type Sand = Position

canMove :: CaveScan -> Sand -> MovePossible 
canMove scan newPos | isOutOfBounds scan newPos             = OUTOFBOUNDS
                    | materialAtPosition scan newPos == AIR = YES
                    | otherwise                             = NO

moveSand :: CaveScan -> Sand -> Maybe Sand
moveSand scan (x,y) | isOutOfBounds scan (x, y)   = Nothing
                    | canMoveDown  == OUTOFBOUNDS = Nothing
                    | canMoveDown  == YES         = Just (x, y+1)
                    | canMoveLeft  == OUTOFBOUNDS = Nothing
                    | canMoveLeft  == YES         = Just (x-1, y+1)
                    | canMoveRight == OUTOFBOUNDS = Nothing
                    | canMoveRight == YES         = Just (x+1, y+1)
                    | otherwise                   = Just (x, y) 
                    where canMoveDown  = canMove scan (x, y+1)
                          canMoveLeft  = canMove scan (x-1, y+1)
                          canMoveRight = canMove scan (x+1, y+1)

data SandState = SandState {
     stateScan :: CaveScan, 
     prevSand  :: Sand,
     nextSand  :: Sand
} deriving (Show)

spawnSand :: CaveScan -> SandState
spawnSand scan = SandState scan (minBound :: Int, minBound :: Int) sandInitPosition

dropSand :: SandState -> Maybe SandState
dropSand (SandState scan prev next) | prev == next && prev /= sandInitPosition = Just $ SandState scan prev next
                                    | prev == next && prev == sandInitPosition = Nothing 
                                    | otherwise                                = case moveSand scan next of
                                                                                      Nothing        -> error "sand hit the abyss!"
                                                                                      Just movedSand -> dropSand $ SandState scan next movedSand

data ScanState = ScanState {
    stateCave :: CaveScan,
    sandCount :: Int
} deriving (Show)

initScanState :: CaveScan -> ScanState
initScanState scan = ScanState scan 0

repeatUntilBlocked :: ScanState -> ScanState
repeatUntilBlocked (ScanState cave sandCount) = case dropSand $ spawnSand cave of
                                                     Nothing        -> ScanState cave (sandCount + 1)
                                                     Just sandState -> repeatUntilBlocked $ ScanState (drawPoint cave (prevSand sandState) SAND) (sandCount + 1)

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content <- lines <$> readFile filename
                                print . sandCount . repeatUntilBlocked . initScanState . initCaveScan . appendFloor . toFormations $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
