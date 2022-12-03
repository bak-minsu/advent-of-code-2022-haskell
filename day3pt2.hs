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
splitMembersHelper l []        = Just l
splitMembersHelper l _         = Nothing

splitMembers :: [String] -> Maybe [(String, String,String)]
splitMembers = splitMembersHelper []

priority :: Char -> Maybe Int
priority c | ord c >= ord 'A' && ord c <= ord 'Z' = Just ((ord c - 64) + 26)
           | ord c >= ord 'a' && ord c <= ord 'z' = Just (ord c - 96)
           | otherwise                            = Nothing

maybePriority :: Maybe Char -> Maybe Int
maybePriority (Just x) = priority x
maybePriority Nothing  = Nothing

uniqueMemberItems :: (String, String, String) -> (String, String, String)
uniqueMemberItems (a, b, c) = (nub a, nub b, nub c)

sortedGroupItems :: (String, String, String) -> String
sortedGroupItems (a, b, c) = sort (a ++ b ++ c) 

findTripled :: String -> Maybe Char
findTripled (x:y:z:t) | x == y && y == z = Just x
                      | otherwise        = findTripled (y:z:t)
findTripled _ = Nothing

findPriority :: (String, String, String) -> Maybe Int
findPriority = maybePriority
               . findTripled
               . sortedGroupItems
               . uniqueMemberItems

findPrioritiesHelper :: Maybe [Int] -> Maybe [(String, String, String)] -> Maybe [Int]
findPrioritiesHelper (Just l) (Just (x:xs)) = 
    case findPriority x of 
        Just priority -> findPrioritiesHelper (Just (priority:l)) (Just xs)
        Nothing -> Nothing
findPrioritiesHelper l (Just []) = l
findPrioritiesHelper Nothing _   = Nothing
findPrioritiesHelper _ Nothing   = Nothing 

findPriorities :: Maybe [(String, String, String)] -> Maybe [Int]
findPriorities = findPrioritiesHelper (Just [])

sumPriorities :: Maybe [Int] -> Maybe Int
sumPriorities (Just l) = Just . sum $ l
sumPriorities Nothing  = Nothing

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content  <- getFileLines filename
                                print . sumPriorities . findPriorities . splitMembers $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
