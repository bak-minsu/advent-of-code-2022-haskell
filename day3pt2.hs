import System.IO
import System.Environment
import Control.Monad
import Data.Char (ord)
import Data.List (sort, nub)

getFileLines :: String -> IO [String]
getFileLines f = do contents <- readFile f
                    return . lines $ contents

splitMembers :: [String] -> Maybe [(String, String, String)]
splitMembers (x:y:z:t) = ((x,y,z):) <$> splitMembers t
splitMembers []        = Just []
splitMembers _         = Nothing

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

findPriorities :: [(String, String, String)] -> Maybe [Int]
findPriorities (x:xs) = case findPriority x of 
                             Nothing -> Nothing
                             Just priority -> (priority:) <$> findPriorities xs
findPriorities [] = Just []

answer :: [String] -> Int
answer xs = case splitMembers xs of
                 Nothing -> error "could not split content into members"
                 Just ys -> case findPriorities ys of
                                 Nothing -> error "could not find priorities"
                                 Just zs -> sum zs

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content  <- getFileLines filename
                                print . answer $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
