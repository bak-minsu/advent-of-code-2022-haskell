import System.IO (readFile, print, putStrLn)
import System.Environment (getArgs)
import Debug.Trace (trace) 
import Data.List (sort)
import Data.List.Split (splitOn)
import Text.Regex.TDFA

allMatches :: String -> String -> [String]
allMatches input regex = getAllTextMatches (input =~ regex) :: [String] 

subMatches :: String -> String -> [String]
subMatches input regex = case input =~ regex :: (String, String, String, [String]) of
                              (_,_,_,matches) -> matches

firstMatch :: String -> String -> String
firstMatch input regex = case input =~ regex :: (String, String, String) of
                              (_, match, _) -> match

readInt :: String -> Int
readInt str = read str :: Int

readInts :: [String] -> [Int]
readInts = map readInt

type Item = Int

divThree :: Item -> Item
divThree n = div n 3

isDivisible :: Item -> Int -> Bool
isDivisible item n = mod item n == 0

type TestValue = Int

type Items = [Item]

data Operation = Operation String (Int -> Int) | Noop

operationFunc :: Operation -> (Int -> Int)
operationFunc (Operation _ f) = f
operationFunc Noop            = error "no operation function for noop"

numberOperation :: String -> (Int -> Int -> Int)
numberOperation "*" = (*)
numberOperation "+" = (+)
numberOperation op  = error ("unsupported operation: " ++ op)

toOperation :: String -> Operation
toOperation str = case subMatches str "(old|[0-9]+) ([+*]) (old|[0-9]+)" of 
                       ["old", op, "old"] -> Operation str (\n -> numberOperation op n n)
                       ["old", op, val]   -> Operation str (\n -> numberOperation op n $ readInt val)
                       [val, op, "old"]   -> Operation str (numberOperation op (readInt val))
                       _                  -> error ("unsupported operation string: " ++ str)

instance Show Operation where
         show Noop                = "noop"
         show (Operation opStr _) = opStr

type MonkeyID = Int

data Monkey = Monkey { 
                monkeyID           :: MonkeyID,
                monkeyInspectCount :: Int,
                monkeyItems        :: Items,
                monkeyOperation    :: Operation,
                monkeyTest         :: TestValue,
                monkeyTrue         :: MonkeyID,
                monkeyFalse        :: MonkeyID
              } deriving (Show)

setMonkeyID :: MonkeyID -> Monkey -> Monkey
setMonkeyID id (Monkey _ count items op test true false) = Monkey id count items op test true false

setMonkeyInspectCount :: Int -> Monkey -> Monkey
setMonkeyInspectCount count (Monkey id _ items op test true false) = Monkey id count items op test true false 

setMonkeyItems :: Items -> Monkey -> Monkey
setMonkeyItems items (Monkey id count _ op test true false) = Monkey id count items op test true false

appendMonkeyItem :: Item -> Monkey -> Monkey
appendMonkeyItem item (Monkey id count items op test true false) = Monkey id count (items ++ [item]) op test true false

setMonkeyOperation :: Operation -> Monkey -> Monkey
setMonkeyOperation op (Monkey id count items _ test true false) = Monkey id count items op test true false

setMonkeyTest :: TestValue -> Monkey -> Monkey
setMonkeyTest test (Monkey id count items op _ true false) = Monkey id count items op test true false

setMonkeyTrue :: MonkeyID -> Monkey -> Monkey
setMonkeyTrue true (Monkey id count items op test _ false) = Monkey id count items op test true false

setMonkeyFalse :: MonkeyID -> Monkey -> Monkey
setMonkeyFalse false (Monkey id count items op test true _) = Monkey id count items op test true false

type MonkeyRecord = [String]

recordToMonkey :: MonkeyRecord -> Monkey
recordToMonkey []           = Monkey 0 0 [] Noop 1 0 0
recordToMonkey (str:record) | str =~ "Monkey [0-9]:" :: Bool                 = setMonkeyID (readInt $ firstMatch str "[0-9]+") $ recordToMonkey record
                            | str =~ "Starting items: ( ?[0-9]+,?)+" :: Bool = setMonkeyItems (readInts $ allMatches str "[0-9]+") $ recordToMonkey record
                            | str =~ "Operation: new = " :: Bool             = setMonkeyOperation (toOperation $ firstMatch str "(old|[0-9]+) ([+*]) (old|[0-9]+)") $ recordToMonkey record
                            | str =~ "Test: divisible by " :: Bool           = setMonkeyTest (readInt $ firstMatch str "[0-9]+") $ recordToMonkey record 
                            | str =~ "If true: throw to monkey " :: Bool     = setMonkeyTrue (readInt $ firstMatch str "[0-9]+") $ recordToMonkey record
                            | str =~ "If false: throw to monkey " :: Bool    = setMonkeyFalse (readInt $ firstMatch str "[0-9]+") $ recordToMonkey record
                            | otherwise                                      = error ("strange record syntax: " ++ str)

type MonkeyRecords = [MonkeyRecord]

splitMonkeyRecords :: [String] -> MonkeyRecords 
splitMonkeyRecords = splitOn [""]

type MonkeyState = [Monkey]

recordsToMonkeyState :: MonkeyRecords -> MonkeyState
recordsToMonkeyState = map recordToMonkey

updateMonkeyWithID :: MonkeyState -> Int -> Monkey -> MonkeyState
updateMonkeyWithID state index monkey = case splitAt index state of
                                             (left, _:right) -> left ++ monkey:right
                                             _               -> error ("index out of bounds: " ++ show index)

topTwoInspectCounts :: MonkeyState -> (Int, Int)
topTwoInspectCounts state = case reverse . sort . map monkeyInspectCount $ state of
                                 (first:second:rest) -> (first, second)
                                 _                   -> error "there needs to be at least two monkeys in state"

type Thrown = (Monkey, [(MonkeyID, Item)])

throwItem :: (MonkeyID, Item) -> Thrown -> Thrown
throwItem item (monkey, thrown) = (monkey, item:thrown)

inspectAndThrow :: Monkey -> Thrown
inspectAndThrow (Monkey id count [] op test true false)           = (Monkey id count [] op test true false, [])
inspectAndThrow (Monkey id count (item:items) op test true false) | isDivisible newItem test = (true, newItem) `throwItem` inspectAndThrow (Monkey id (count+1) items op test true false)
                                                                  | otherwise                = (false, newItem) `throwItem` inspectAndThrow (Monkey id (count+1) items op test true false)
                                                                  where newItem = divThree $ operationFunc op item

catchItems :: MonkeyState -> Thrown -> MonkeyState
catchItems state (monkey, [])                = updateMonkeyWithID state (monkeyID monkey) monkey
catchItems state (monkey, (id, item):thrown) = updateMonkeyWithID state id (appendMonkeyItem item (state !! id)) `catchItems` (monkey, thrown)

type InspectingMonkey = (MonkeyState, MonkeyID)

initInspectingMonkey :: MonkeyState -> InspectingMonkey
initInspectingMonkey state = (state, 0)

executeRound :: InspectingMonkey -> InspectingMonkey
executeRound (state, id) | id < length state  = executeRound (catchItems state $ inspectAndThrow (state !! id), id + 1)
                         | id == length state = (state, 0)
                         | otherwise          = error ("out of bounds for round execution: " ++ show id)

executeRounds :: Int -> InspectingMonkey -> InspectingMonkey
executeRounds 0      inspect = inspect
executeRounds rounds inspect = executeRounds (rounds - 1) $ executeRound inspect

calculateLevel :: InspectingMonkey -> Int
calculateLevel (state, _) = case topTwoInspectCounts state of
                                 (first, second) -> first * second

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content <- lines <$> readFile filename
                                print . calculateLevel . executeRounds 20 . initInspectingMonkey . recordsToMonkeyState . splitMonkeyRecords $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
