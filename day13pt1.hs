import System.IO (readFile, print, putStrLn)
import System.Environment (getArgs)
import Debug.Trace (trace) 
import Data.List.Split (splitOn)
import Text.Regex.TDFA
import Data.Char (isDigit)

convPair :: [String] -> (String, String)
convPair strs | length strs /= 2 = error "cannot turn non-pair list into pair"
              | otherwise        = (head strs, last strs)

convPairs :: [String] -> [(String, String)]
convPairs content = map convPair $ splitOn [""] content

data ListItem = List [ListItem] | Item Int deriving (Show)

data ListPath = ListPath ListPath ListItem | Root deriving (Show)

type TraverseState = (ListPath, [ListItem], String)

goDown :: TraverseState -> TraverseState
goDown (parentPath, currentList, _) = (ListPath parentPath (List currentList), [], [])

goUp :: TraverseState -> TraverseState
goUp (Root, _, _)                     = error "could not go above Root"
goUp (ListPath _ (Item _), _, _)      = error "could not go up to Item"
goUp (ListPath path (List parentList), currentList, _) = (path, List (reverse currentList) : parentList, []) 

appendStrItem :: String -> [ListItem] -> [ListItem]
appendStrItem ""  list = list
appendStrItem str list = Item (read str :: Int) : list

toListItemState :: TraverseState -> String -> ListItem
toListItemState (_, currentList, _)             []          = head currentList
toListItemState state                           ('[':rest)  = goDown state `toListItemState` rest
toListItemState (parentList, currentList, item) (']':rest)  = goUp (parentList, item `appendStrItem` currentList, []) `toListItemState` rest
toListItemState (parentList, currentList, item) (',':rest)  = (parentList, item `appendStrItem` currentList, []) `toListItemState` rest
toListItemState (parentList, currentList, item) (char:rest) = (parentList, currentList, item ++ [char]) `toListItemState` rest

toListItem :: String -> ListItem
toListItem = toListItemState (Root, [], []) 

type ListItemPair = (ListItem, ListItem)

toListItemPair :: (String, String) -> ListItemPair
toListItemPair (strLeft, strRight) = (toListItem strLeft, toListItem strRight)

toListItemPairs :: [(String, String)] -> [ListItemPair]
toListItemPairs = map toListItemPair

compareItems :: ListItemPair -> Ordering
compareItems (Item leftItem, Item rightItem) = compare leftItem rightItem
compareItems (List []      , List []       ) = EQ
compareItems (List []      , List rightList) = LT
compareItems (List leftList, List []       ) = GT
compareItems (Item leftItem, List rightList) = compareItems (List [Item leftItem], List rightList)
compareItems (List leftList, Item rightItem) = compareItems (List leftList, List [Item rightItem])
compareItems (List leftList, List rightList) | compareResult == EQ = compareItems (List $ tail leftList, List $ tail rightList) 
                                             | otherwise           = compareResult
                                             where compareResult = compareItems (head leftList, head rightList)

findCorrectIndices :: [ListItemPair] -> [Int]
findCorrectIndices pairs = map fst $ filter ((GT /=) . snd) $ zip [1..] $ map compareItems pairs

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content <- lines <$> readFile filename
                                print . sum . findCorrectIndices . toListItemPairs . convPairs $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
