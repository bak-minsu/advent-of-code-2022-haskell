import System.IO (readFile, print, putStrLn)
import System.Environment (getArgs)
import Data.List (nub)

getFileLines :: String -> IO [String]
getFileLines f = do contents <- readFile f
                    return . lines $ contents

allUnique :: [Char] -> Bool
allUnique xs = length (nub xs) == 14

uniqueIndex :: String -> Int
uniqueIndex xs = if allUnique . take 14 $ xs
                 then 14
                 else 1 + (uniqueIndex . drop 1 $ xs)

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content  <- getFileLines filename
                                print . map uniqueIndex $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
