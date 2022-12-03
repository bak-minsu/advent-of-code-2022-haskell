import System.IO
import System.Environment
import Control.Monad
import Data.List

getFileLines :: String -> IO [String]
getFileLines f = do contents <- readFile f
                    return . lines $ contents

convInt :: String -> Int
convInt s = read s :: Int

elfListHelper :: [[Int]] -> [String] -> [[Int]]
elfListHelper t [] = t
elfListHelper [] (x:xs) = elfListHelper [[convInt x]] xs
elfListHelper t ("":xs) = elfListHelper ([]:t) xs
elfListHelper (h:t) (x:xs) = elfListHelper ((convInt x:h):t) xs

elfList :: [String] -> [[Int]]
elfList = elfListHelper []

elfCalories :: [String] -> [Int]
elfCalories = map sum . elfList

findMax :: [Int] -> Int
findMax = foldl max 0

sumTopThree :: [Int] -> Int
sumTopThree = sum . take 3 . reverse . sort

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content <- getFileLines filename
                                print . sumTopThree . elfCalories $ content
               _          -> putStrLn "Usage: aoc2022 <filename>" 
                

