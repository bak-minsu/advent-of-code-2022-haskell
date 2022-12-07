import System.IO (readFile, print, putStrLn)
import System.Environment (getArgs)
import Data.List (elemIndex)
import Data.List.Split (chunksOf, splitOn)

getFileLines :: String -> IO [String]
getFileLines f = do contents <- readFile f
                    return . lines $ contents

data Crate = Crate Char | Air deriving (Show)

crateChar :: Crate -> Char
crateChar (Crate x) = x
crateChar Air       = error "cannot get character of Air"

removeSpace :: String -> String
removeSpace (' ':xs) = removeSpace xs
removeSpace (x:xs)   = x:removeSpace xs
removeSpace []       = ""

convCrate :: String -> Crate
convCrate ['[', x, ']'] = Crate x
convCrate []            = Air
convCrate _             = error "incorrect crate format" 

convCrateLine :: String -> [Crate]
convCrateLine = map (convCrate . removeSpace) . chunksOf 4

newtype Stack = Stack [Crate] deriving (Show)

pushStackMulti :: Stack -> [Crate] -> Stack
pushStackMulti (Stack stack) xs = Stack (stack ++ xs)

popStackMulti :: Stack -> Int -> Maybe (Stack, [Crate])
popStackMulti (Stack stack) n | length stack >= n = Just (Stack $ take (length stack - n) stack, drop (length stack - n) stack)
                              | otherwise         = Nothing

readStack :: Stack -> Maybe Crate
readStack (Stack []) = Nothing
readStack (Stack xs) = Just $ last xs

data Movement = Movement {
    moveCount :: Int,
    fromStack :: Int,
    toStack   :: Int
} deriving (Show)

readInt :: String -> Int
readInt s = read s ::Int

convMovement :: String -> Movement
convMovement s = case splitOn " " s of
                      ["move", move, "from", from, "to", to] -> Movement (readInt move) (readInt from-1) (readInt to-1)
                      _                                      -> error "incorrect movement format"

readMovements :: [String] -> [Movement]
readMovements = map convMovement 

updateStacksIndex :: [Stack] -> (Int, Stack) -> [Stack]
updateStacksIndex stacks (i, stack) = take i stacks ++ stack:drop (i+1) stacks

updateStacksIndex2 :: [Stack] -> (Int, Stack) -> (Int, Stack) -> [Stack]
updateStacksIndex2 stacks input1 input2 = foldl updateStacksIndex stacks [input1, input2]

moveCrate :: [Stack] -> Movement -> [Stack]
moveCrate stacks (Movement move from to) = case popStackMulti (stacks!!from) move of
                                                Nothing              -> error "cannot move that many crates"
                                                Just (stack, crates) -> updateStacksIndex2 stacks (to, pushStackMulti (stacks!!to) crates) (from, stack)

completeMoves :: [Stack] -> [Movement] -> [Stack]
completeMoves = foldl moveCrate

splitInputTypes :: [String] -> ([String], String, [String])
splitInputTypes l = case elemIndex "" l of
                         Nothing -> error "could not split input"
                         Just i  -> (take (i-1) l, l !! (i-1), drop (i+1) l)

pushCratesOnly :: Stack -> Crate -> Stack
pushCratesOnly stack Air           = stack
pushCratesOnly (Stack stack) crate = Stack (crate:stack)

pushCratesLine :: [Stack] -> [Crate] -> [Stack]
pushCratesLine = zipWith pushCratesOnly

pushStringCrates :: [Stack] -> String -> [Stack]
pushStringCrates stacks = pushCratesLine stacks . convCrateLine

initStacks :: [Stack] -> [String] -> [Stack]
initStacks = foldl pushStringCrates

blankStacks :: Int -> [Stack]
blankStacks n = replicate n $ Stack []

skipToSpace :: String -> Int
skipToSpace (' ':xs) = getStackCount (' ':xs)
skipToSpace (x:xs)   = skipToSpace xs
skipToSpace []       = 0

getStackCount :: String -> Int
getStackCount (' ':xs) = getStackCount xs
getStackCount (x:xs)   = 1 + skipToSpace xs
getStackCount []       = 0

readInitStacks :: [String] -> String -> [Stack]
readInitStacks crates labels = initStacks (blankStacks . getStackCount $ labels) crates

topValues :: [Stack] -> String
topValues (x:xs) = case readStack x of
                        Nothing    -> error "cannot get top value of empty stack"
                        Just crate -> crateChar crate:topValues xs
topValues []     = ""

debug :: [String] -> String
debug l = case splitInputTypes l of
               (crates, labels, moves) -> show . topValues $ completeMoves (readInitStacks crates labels) $ readMovements moves

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content  <- getFileLines filename
                                print . debug $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
