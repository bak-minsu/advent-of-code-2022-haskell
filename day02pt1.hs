import System.IO
import System.Environment
import Control.Monad

getFileLines :: String -> IO [String]
getFileLines f = do contents <- readFile f
                    return . lines $ contents


data Shape = Shape {
    them :: String,
    you :: String,
    common :: String,
    shapeScore :: Int,
    win :: String
  } deriving (Eq, Show)

rock :: Shape
rock = Shape "A" "X" "Rock" 1 "Scissors"

paper :: Shape
paper = Shape "B" "Y" "Paper" 2 "Rock" 

scissors :: Shape
scissors = Shape "C" "Z" "Scissors" 3 "Paper"

convShape :: String -> Shape
convShape s | you rock == s || them rock == s = rock
            | you paper == s || them paper == s = paper
            | otherwise = scissors

getRound :: String -> (Shape, Shape)
getRound l = (convShape $ head . words $ l, convShape $ words l !! 1)

rounds :: [String] -> [(Shape, Shape)]
rounds = map getRound 

newtype Outcome = Outcome { outcomeScore :: Int }
  deriving (Eq, Show)

lost :: Outcome
lost = Outcome 0

tied :: Outcome
tied = Outcome 3

won :: Outcome
won = Outcome 6

outcome :: (Shape, Shape) -> Outcome
outcome (them, you) | win you == common them = won
                    | common you == common them = tied
                    | otherwise = lost

score :: (Shape, Shape) -> Int
score (their, your) = shapeScore your + (outcomeScore . outcome $ (their, your)) 

total :: [(Shape, Shape)] -> Int
total = sum . map score

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content <- getFileLines filename
                                print . total . rounds $ content
               _          -> putStrLn "Usage: aoc2022 <filename>" 
                

