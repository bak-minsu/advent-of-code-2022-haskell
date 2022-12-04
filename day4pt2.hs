import System.IO
import System.Environment
import Control.Monad
import Data.List.Split (splitOn)

getFileLines :: String -> IO [String]
getFileLines f = do contents <- readFile f
                    return . lines $ contents

convInt :: String -> Int
convInt s = read s :: Int

data Range = Range Int Int deriving (Show)

convRange :: String -> Range
convRange s = case splitOn "-" s of
                [x, y] -> Range (convInt x) (convInt y)
                _      -> error "could not convert string into range" 

data Pair = Pair Range Range deriving (Show)

convPair :: String -> Pair
convPair s = case splitOn "," s of
               [x, y] -> Pair (convRange x) (convRange y)
               _      -> error "could not convert string into pair" 

convPairs :: [String] -> [Pair]
convPairs = map convPair

hasOverlap :: Pair -> Bool
hasOverlap (Pair (Range x1 x2) (Range y1 y2)) | x1 < y1   = x2 >= y1
                                              | y1 < x1   = y2 >= x1
                                              | x1 == y1  = True
                                              | otherwise = False 

countOverlap :: [Pair] -> Int
countOverlap = length . filter (== True) . map hasOverlap

debugOutput :: [String] -> String
debugOutput c = unlines . zipWith (++) (map (show . hasOverlap) . convPairs $ c) $ (map show . convPairs $ c)

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content  <- getFileLines filename
                                print . countOverlap . convPairs $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
