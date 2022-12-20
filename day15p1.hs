import System.IO (readFile, print, putStrLn)
import System.Environment (getArgs)
import Debug.Trace (trace)
import Text.Regex.TDFA
import Data.List (nub, sort)
import Data.Maybe (mapMaybe)

data Sensor = Sensor { 
    sensorX :: Int,
    sensorY :: Int 
} deriving (Show, Eq)

data Beacon = Beacon {
    beaconX :: Int,
    beaconY :: Int
} deriving (Show, Eq)

beaconSearchY :: Int
beaconSearchY = 2000000

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

beaconAtY :: [SensorData] -> [Beacon]
beaconAtY sensorData = nub . filter ((== beaconSearchY) . beaconY) $ map dataBeacon sensorData

distance :: SensorData -> Int
distance (SensorData (Sensor x1 y1) (Beacon x2 y2)) = abs (x2 - x1) + abs (y2 - y1)

toRange :: SensorData -> Maybe Range
toRange sensorData | yComponent == 0 = Just $ Range xSensor xSensor
                   | yComponent >  0 = Just $ Range (xSensor - yComponent) (xSensor + yComponent)
                   | otherwise       = Nothing
                   where yComponent  = distance sensorData - abs (beaconSearchY - sensorY (dataSensor sensorData))
                         xSensor     = sensorX (dataSensor sensorData)

data Range = Range Int Int deriving (Show, Eq)

instance Ord Range where
    compare (Range startLeft endLeft) (Range startRight endRight) = case compare startLeft startRight of
                                                                         EQ    -> compare endLeft endRight
                                                                         other -> other

toRanges :: [SensorData] -> [Range]
toRanges = mapMaybe toRange

reduceOrderedRanges :: [Range] -> [Range]
reduceOrderedRanges []                                                       = []
reduceOrderedRanges [range]                                                  = [range]
reduceOrderedRanges (Range startLeft endLeft:Range startRight endRight:rest) | endLeft < startRight = Range startLeft endLeft:reduceOrderedRanges (Range startRight endRight:rest)
                                                                             | otherwise            = reduceOrderedRanges (Range startLeft (max endLeft endRight):rest)

reduceRanges :: [Range] -> [Range]
reduceRanges ranges = reduceOrderedRanges $ sort ranges

removeBeacon :: [Range] -> Beacon -> [Range]
removeBeacon []                     _            = []
removeBeacon (Range start end:rest) (Beacon x y) | x == start && x == end = removeBeacon rest (Beacon x y)
                                                 | x == start && x <  end = Range (x+1) end:removeBeacon rest (Beacon x y)
                                                 | x >  start && x == end = Range start (x-1):removeBeacon rest (Beacon x y)
                                                 | x >  start && x <  end = Range start (x-1):Range (x+1) end:removeBeacon rest (Beacon x y)
                                                 | otherwise              = Range start end:removeBeacon rest (Beacon x y)

toRangesWithoutBeacon :: [SensorData] -> [Range]
toRangesWithoutBeacon sensorData = foldl removeBeacon (reduceRanges $ toRanges sensorData) (beaconAtY sensorData)

sumRange :: Range -> Int
sumRange (Range start end) = abs (end - start) + 1

sumRanges :: [Range] -> Int
sumRanges = sum . map sumRange

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content <- lines <$> readFile filename
                                print . sumRanges . toRangesWithoutBeacon . readSensorData $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
