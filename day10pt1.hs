import System.IO (readFile, print, putStrLn)
import System.Environment (getArgs)
import Debug.Trace (trace) 
import Data.List.Split (splitOn)

data Instruction = Add Int | Noop deriving (Show)

toInstruction :: String -> Instruction
toInstruction "noop" = Noop
toInstruction str    = case splitOn " " str of
                            ["addx", num] -> Add (read num :: Int)
                            _             -> error (str ++ " is not a valid instruction")

toInstructions :: [String] -> [Instruction]
toInstructions = map toInstruction

type Cycles = Int

type Register = Int

type CycleHistory = [Register]

initState :: CycleHistory
initState = [1]

executeInstruction :: CycleHistory -> Instruction -> CycleHistory
executeInstruction []                _       = error "state history needs at least one member"
executeInstruction (register:states) (Add x) = register+x:register:register:states
executeInstruction (register:states) Noop    = register:register:states

executeProgram :: CycleHistory -> [Instruction] -> CycleHistory
executeProgram state instructions = reverse $ foldl executeInstruction state instructions

statesAt :: [Int] -> CycleHistory -> CycleHistory
statesAt []      history = []
statesAt indices history = map ((history !!) . decrement) indices

decrement :: Int -> Int
decrement n = n - 1

stateIndices :: [Int]
stateIndices = [20, 60, 100, 140, 180, 220]

calculateSignalStrength :: CycleHistory -> Int
calculateSignalStrength history = sum $ zipWith (*) stateIndices $ statesAt stateIndices history

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content <- lines <$> readFile filename
                                print . calculateSignalStrength .  executeProgram initState . toInstructions $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
