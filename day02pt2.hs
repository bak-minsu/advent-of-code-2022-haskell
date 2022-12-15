import System.IO
import System.Environment
import Control.Monad

getFileLines :: String -> IO [String]
getFileLines f = do contents <- readFile f
                    return . lines $ contents

data Shape = Rock | Paper | Scissors deriving (Eq, Enum, Show)

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

convShape :: String -> Maybe Shape
convShape "A" = Just Rock
convShape "B" = Just Paper
convShape "C" = Just Scissors
convShape _ = Nothing

data Outcome = Won | Tied | Lost deriving (Eq, Enum, Show)

outcomeScore :: Outcome -> Int
outcomeScore Won = 6
outcomeScore Tied = 3
outcomeScore Lost = 0

convOutcome :: String -> Maybe Outcome
convOutcome "X" = Just Lost
convOutcome "Y" = Just Tied
convOutcome "Z" = Just Won
convOutcome _ = Nothing

convRound :: [String] -> Maybe (Shape, Outcome)
convRound [s, o] = case (convShape s, convOutcome o) of
                     (Just js, Just jo) -> Just (js, jo)
                     _                  -> Nothing
convRound _ = Nothing

yourMove :: (Shape, Outcome) -> Shape
yourMove (Rock, Won) = Paper
yourMove (Rock, Tied) = Rock
yourMove (Rock, Lost) = Scissors
yourMove (Paper, Won) = Scissors
yourMove (Paper, Tied) = Paper
yourMove (Paper, Lost) = Rock
yourMove (Scissors, Won) = Rock
yourMove (Scissors, Tied) = Scissors
yourMove (Scissors, Lost) = Paper

maybeRounds :: Maybe [(Shape, Outcome)] -> [Maybe (Shape, Outcome)] -> Maybe [(Shape, Outcome)]
maybeRounds (Just l) (Just x:xs) = maybeRounds (Just $ x:l) xs
maybeRounds (Just l) [] = Just l
maybeRounds Nothing _ = Nothing
maybeRounds _ (Nothing:_) = Nothing

rounds :: [String] -> Maybe [(Shape, Outcome)]
rounds = maybeRounds (Just []) . map (convRound . words)

score :: (Shape, Outcome) -> Int
score (s, o) = outcomeScore o + (shapeScore . yourMove $ (s, o)) 

total :: Maybe [(Shape, Outcome)] -> Maybe Int
total (Just rs) = Just . sum . map score $ rs
total Nothing = Nothing

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content <- getFileLines filename
                                print . total . rounds $ content
               _          -> putStrLn "Usage: aoc2022 <filename>" 
                

