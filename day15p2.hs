import System.IO (readFile, print, putStrLn)
import System.Environment (getArgs)
import Debug.Trace (trace)
import Text.Regex.TDFA
import Data.List (nub, sort, findIndices)
import Data.Maybe (mapMaybe, catMaybes)

maxSearchSpace :: Int
maxSearchSpace = 4000000 

data Sensor = Sensor { 
    sensorX :: Int,
    sensorY :: Int 
} deriving (Show, Eq)

data Beacon = Beacon {
    beaconX :: Int,
    beaconY :: Int
} deriving (Show, Eq)

tuningFrequency :: Beacon -> Int
tuningFrequency (Beacon x y) = (x * 4000000) + y

data SensorData = SensorData {
    dataSensor   :: Sensor,
    dataBeacon   :: Beacon
} deriving (Show)

readInt :: String -> Int
readInt str = read str :: Int

toSensorData :: String -> SensorData
toSensorData str = case getAllTextMatches (str =~ "-?[0-9]+") :: [String] of
                          [sensorX, sensorY, beaconX, beaconY] -> SensorData (Sensor (readInt sensorX) (readInt sensorY)) (Beacon (readInt beaconX) (readInt beaconY))
                          _                                    -> error "there should be four numbers"

readSensorData :: [String] -> [SensorData]
readSensorData = map toSensorData

distance :: SensorData -> Int
distance (SensorData (Sensor x1 y1) (Beacon x2 y2)) = abs (x2 - x1) + abs (y2 - y1)

data Range = Range Int Int deriving (Show, Eq)

toRange :: SensorData -> Int -> Maybe Range
toRange sensorData y | yComponent == 0 = Just $ Range xSensor xSensor
                     | yComponent >  0 = Just $ Range (xSensor - yComponent) (xSensor + yComponent)
                     | otherwise       = Nothing
                     where yComponent  = distance sensorData - abs (y - sensorY (dataSensor sensorData))
                           xSensor     = sensorX (dataSensor sensorData)

instance Ord Range where
    compare (Range startLeft endLeft) (Range startRight endRight) = case compare startLeft startRight of
                                                                         EQ    -> compare endLeft endRight
                                                                         other -> other

toRangesXAxis :: [SensorData] -> Int -> [Range]
toRangesXAxis sensorData x = mapMaybe (`toRange` x) sensorData

reduceOrderedRanges :: [Range] -> [Range]
reduceOrderedRanges []                                                       = []
reduceOrderedRanges [range]                                                  = [range]
reduceOrderedRanges (Range startLeft endLeft:Range startRight endRight:rest) | endLeft < startRight = Range startLeft endLeft:reduceOrderedRanges (Range startRight endRight:rest)
                                                                             | otherwise            = reduceOrderedRanges (Range startLeft (max endLeft endRight):rest)

reduceRanges :: [Range] -> [Range]
reduceRanges ranges = reduceOrderedRanges $ sort ranges

type XAxisRanges = [[Range]]

toAllXAxisRanges :: [SensorData] -> XAxisRanges
toAllXAxisRanges sensorData = map (reduceRanges . toRangesXAxis sensorData) [0..maxSearchSpace]

type BeaconSearchState = (XAxisRanges, Int)

initBeaconSearchState :: XAxisRanges -> BeaconSearchState
initBeaconSearchState xRanges = (xRanges, 0)

findBeacon :: BeaconSearchState -> Beacon
findBeacon ([], _)                           = error "could not find beacon"
findBeacon ([Range _ x1, Range _ _]:rest, y) = Beacon (x1 + 1) y
findBeacon (ranges:rest, y)                  = findBeacon (rest, y+1)

sumRange :: Range -> Int
sumRange (Range start end) = abs (end - start) + 1

sumRanges :: [Range] -> Int
sumRanges = sum . map sumRange

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content <- lines <$> readFile filename
                                print . tuningFrequency . findBeacon . initBeaconSearchState . toAllXAxisRanges . readSensorData $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
