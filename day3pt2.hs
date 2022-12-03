import System.IO
import System.Environment
import Control.Monad
import Data.Char (ord)
import Data.List (sort, nub)

getFileLines :: String -> IO [String]
getFileLines f = do contents <- readFile f
                    return . lines $ contents

splitMembersHelper :: [(String,String,String)] -> [String] -> Maybe [(String, String, String)]
splitMembersHelper l (x:y:z:t) = splitMembersHelper ((x,y,z):l) t
splitMembersHelper l [] = Just l
splitMembersHelper l _ = Nothing

splitMembers :: [String] -> Maybe [(String, String,String)]
splitMembers = splitMembersHelper []

priority :: Char -> Maybe Int
priority c | ord c >= ord 'A' && ord c <= ord 'Z' = Just ((ord c - 64) + 26)
           | ord c >= ord 'a' && ord c <= ord 'z' = Just (ord c - 96)
           | otherwise = Nothing

maybePriority :: Maybe Char -> Maybe Int
maybePriority (Just x) = priority x
maybePriority Nothing  = Nothing

maybeIntsHelper :: Maybe [Int] -> [Maybe Int] -> Maybe [Int]
maybeIntsHelper (Just l) (Just x:xs) = maybeIntsHelper (Just (x:l)) xs
maybeIntsHelper l []                 = l
maybeIntsHelper Nothing _            = Nothing
maybeIntsHelper _ (Nothing:xs)       = Nothing

maybeInts :: [Maybe Int] -> Maybe [Int]
maybeInts = maybeIntsHelper (Just [])

uniqueMemberItems :: (String, String, String) -> (String, String, String)
uniqueMemberItems (a, b, c) = (nub a, nub b, nub c)

sortedGroupItems :: (String, String, String) -> String
sortedGroupItems (a, b, c) = sort (a ++ b ++ c) 

findTripled :: String -> Maybe Char
findTripled (x:y:z:t) | x == y && y == z = Just x
                      | otherwise        = findTripled (y:z:t)
findTripled _ = Nothing

findPrioritiesHelper :: [Maybe Int] -> Maybe [(String, String, String)] -> Maybe [Int]
findPrioritiesHelper l (Just (x:xs)) =  findPrioritiesHelper ((maybePriority . findTripled . sortedGroupItems . uniqueMemberItems $ x):l) (Just xs)
findPrioritiesHelper l (Just [])     = maybeInts l
findPrioritiesHelper l Nothing       = Nothing 

findPriorities :: Maybe [(String, String, String)] -> Maybe [Int]
findPriorities = findPrioritiesHelper []

sumPriorities :: Maybe [Int] -> Maybe Int
sumPriorities (Just l) = Just . sum $ l
sumPriorities Nothing  = Nothing

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content  <- getFileLines filename
                                print . sumPriorities . findPriorities . splitMembers $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
