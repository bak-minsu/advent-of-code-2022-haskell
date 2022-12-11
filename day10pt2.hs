import System.IO (readFile, print, putStrLn)
import System.Environment (getArgs)
import Debug.Trace (trace) 
import Data.List.Split (splitOn, chunksOf)

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

type ScanLines = [Register]

type CRT = [ScanLines]

toCRTs :: CycleHistory -> [CRT]
toCRTs = chunksOf 6 . chunksOf 40

maxYCRT :: CRT -> Int
maxYCRT crt = length crt - 1

maxXCRT :: CRT -> Int
maxXCRT crt = foldl min maxBound (map length crt) - 1

type Position = (Int, Int)

type CRTPosition = (CRT, Position)

initCRTPosition :: CRT -> CRTPosition
initCRTPosition crt = (crt, (0,0))

initCRTPositions :: [CRT] -> [CRTPosition]
initCRTPositions = map initCRTPosition

registerAtCRTPosition :: CRTPosition -> Register
registerAtCRTPosition (crt, (x,y)) = (crt !! y) !! x

lightCRT :: CRTPosition -> Int -> Bool
lightCRT position x | registerValue + 1 == x || registerValue - 1 == x || registerValue == x = True
                    | otherwise                                                              = False
                    where registerValue = registerAtCRTPosition position

charCRT :: CRTPosition -> Char
charCRT (crt, (x,y)) | lightCRT (crt, (x,y)) x = '#'
                     | otherwise               = '.'

stringCRT :: CRTPosition -> String 
stringCRT (crt, (x, y)) | x == maxXCRT crt && y == maxYCRT crt = [charCRT (crt, (x, y))]
                        | x < maxXCRT crt                      = charCRT (crt, (x, y)) : stringCRT (crt, (x + 1,y))
                        | x == maxXCRT crt                     = charCRT (crt, (x, y)) : '\n' : stringCRT (crt, (0,y+1))
                        | otherwise                            = error (show (x, y) ++ " coords were out bounds") 

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content <- lines <$> readFile filename
                                putStr . unlines . map stringCRT . initCRTPositions . toCRTs . executeProgram initState . toInstructions $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
