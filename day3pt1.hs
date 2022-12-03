import System.IO
import System.Environment
import Control.Monad
import Data.Char (ord)
import Data.List (sort)

getFileLines :: String -> IO [String]
getFileLines f = do contents <- readFile f
                    return . lines $ contents

splitHalf :: String -> (String, String)
splitHalf s = splitAt (div (length s) 2) s

splitHalfAll :: [String] -> [(String, String)]
splitHalfAll = map splitHalf

priority :: Char -> Maybe Int
priority c | ord c >= ord 'A' && ord c <= ord 'Z' = Just ((ord c - 64) + 26)
           | ord c >= ord 'a' && ord c <= ord 'z' = Just (ord c - 96)
           | otherwise = Nothing

maybeIntsHelper :: Maybe [Int] -> [Maybe Int] -> Maybe [Int]
maybeIntsHelper (Just l) (Just x:xs) = maybeIntsHelper (Just (x:l)) xs
maybeIntsHelper l [] = l
maybeIntsHelper Nothing _ = Nothing
maybeIntsHelper _ (Nothing:xs) = Nothing

maybeInts :: [Maybe Int] -> Maybe [Int]
maybeInts = maybeIntsHelper (Just [])

convPriority :: String -> Maybe [Int]
convPriority = maybeInts . map priority 

findPriorityHelper :: Int -> Int -> [Int] -> [Int] -> Maybe Int
findPriorityHelper i j x y | (x!!i) == (y!!j) = Just $ x!!i
                           | (x!!i) > (y!!j)  = findPriorityHelper i (j+1) x y
                           | (x!!i) < (y!!j)  = findPriorityHelper (i+1) j x y
                           | otherwise        = Nothing

findPriority :: Maybe [Int] -> Maybe [Int] -> Maybe Int
findPriority (Just x) (Just y) = findPriorityHelper 0 0 (sort x) (sort y)
findPriority _ _ = Nothing

findPrioritiesHelper :: [Maybe Int] -> [(String, String)] -> Maybe [Int]
findPrioritiesHelper l ((lft, rt):xs) = findPrioritiesHelper (findPriority (convPriority lft) (convPriority rt):l) xs
findPrioritiesHelper l [] = maybeInts l

findPriorities :: [String] -> Maybe [Int]
findPriorities l = findPrioritiesHelper [] (splitHalfAll l)

sumPriorities :: Maybe [Int] -> Maybe Int
sumPriorities (Just l) = Just . sum $ l
sumPriorities Nothing = Nothing

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content <- getFileLines filename
                                print . sumPriorities . findPriorities $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
