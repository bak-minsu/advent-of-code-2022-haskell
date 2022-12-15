import System.IO (readFile, print, putStrLn)
import System.Environment (getArgs)
import Data.List (nub)

getFileLines :: String -> IO [String]
getFileLines f = do contents <- readFile f
                    return . lines $ contents

allUnique :: (Char, Char, Char, Char) -> Bool
allUnique (a, b, c, d) = length (nub [a,b,c,d]) == 4

uniqueIndex :: String -> Int
uniqueIndex (a:b:c:d:xs) = if allUnique (a, b, c, d)
                           then 4
                           else 1 + uniqueIndex (b:c:d:xs)
uniqueIndex _            = error "could not find unique sequence" 

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content  <- getFileLines filename
                                print . map uniqueIndex $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
