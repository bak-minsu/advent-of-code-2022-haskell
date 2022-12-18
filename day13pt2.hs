import System.IO (readFile, print, putStrLn)
import System.Environment (getArgs)
import Debug.Trace (trace) 
import Data.List.Split (splitOn)
import Text.Regex.TDFA
import Data.List (sort)

data ListItem = List [ListItem] | Item Int deriving (Show, Eq)

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

dividerPackets :: (ListItem, ListItem)
dividerPackets = (List [List [Item 6]], List [List [Item 2]])

isDividerPacket :: ListItem -> Bool
isDividerPacket item = (fst dividerPackets == item) || (snd dividerPackets == item)

toListItems :: [String] -> [ListItem]
toListItems strs = fst dividerPackets:snd dividerPackets:map toListItem (filter ("" /=) strs)

compareItems :: ListItem -> ListItem -> Ordering
compareItems (Item leftItem) (Item rightItem) = compare leftItem rightItem
compareItems (List []      ) (List []       ) = EQ
compareItems (List []      ) (List rightList) = LT
compareItems (List leftList) (List []       ) = GT
compareItems (Item leftItem) (List rightList) = compareItems (List [Item leftItem]) (List rightList)
compareItems (List leftList) (Item rightItem) = compareItems (List leftList) (List [Item rightItem])
compareItems (List leftList) (List rightList) | compareResult == EQ = compareItems (List $ tail leftList) (List $ tail rightList) 
                                              | otherwise           = compareResult
                                              where compareResult = compareItems (head leftList) (head rightList)

instance Ord ListItem where
    compare left right = compareItems left right

findDividerPacketIndices :: [ListItem] -> [Int]
findDividerPacketIndices items = map fst $ filter (isDividerPacket . snd) $ zip [1..] items

main :: IO ()
main = do args <- getArgs
          case args of
               [filename] -> do content <- lines <$> readFile filename
                                print . product . findDividerPacketIndices . sort . toListItems $ content
               _          -> putStrLn "Usage: aoc2022 <filename>"
