{-# LANGUAGE CPP, TemplateHaskell #-}

module Sudoku where

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Gen
import Data.Maybe
import Data.List
import Data.List.Split
import qualified Data.Sequence as DS
import Data.Foldable (toList)

runTests = $quickCheckAll

boardSize = 9
squareSize = 3

type Field = Maybe Int

newtype Board = Board (DS.Seq Field)

instance Show Board where
  show (Board b) = intercalate "\n" rows
    where
      rowItems :: [[String]]
      rowItems = chunksOf boardSize (map render (toList b))
      injectPipes :: [String] -> String
      injectPipes s = intercalate "|" $ chunksOf squareSize (concat s)
      sepRowItems :: [String]
      sepRowItems = map injectPipes rowItems
      rows = intercalate ["---+---+---"] $ chunksOf squareSize sepRowItems
      render Nothing = "."
      render (Just i) = show i

data Pos = Pos Int Int deriving Show

posGen :: Gen Pos
posGen =
  do
    r <- choose (0, boardSize - 1)
    c <- choose (0, boardSize - 1)
    return (Pos r c)

instance Arbitrary Pos where
  arbitrary = posGen

idxToPos :: Int -> Pos
idxToPos i = Pos (i `div` boardSize) (i `mod` boardSize)

posToIdx :: Pos -> Int
posToIdx (Pos r c) = r * boardSize + c

field :: Board -> Pos -> Field
field (Board b) p = DS.index b (posToIdx p)

prop_field pos @ (Pos r c) =
  field (Board board) pos == (Just $ r * boardSize + c)
  where board = DS.fromList $ map (\i -> Just i) (take (boardSize ^ 2) [0..])

rowVals :: Board -> Pos -> [Int]
rowVals b (Pos r c) = catMaybes [field b (Pos r c') | c' <- take boardSize [0..]]

prop_rowVals pos @ (Pos r _) =
  sort rv == take boardSize [(r * boardSize)..]
  where board = DS.fromList $ map (\i -> Just i) (take (boardSize ^ 2) [0..])
        rv = rowVals (Board board) pos

colVals :: Board -> Pos -> [Int]
colVals b (Pos r c) = catMaybes [field b (Pos r' c) | r' <- take boardSize [0..]]

prop_colVals pos @ (Pos _ c) =
  sort cv == [v * boardSize + c | v <- take boardSize [0..]]
  where board = DS.fromList $  map (\i -> Just i) (take (boardSize ^ 2) [0..])
        cv = colVals (Board board) pos

squareVals :: Board -> Pos -> [Int]
squareVals b (Pos r c) =
  catMaybes [field b (Pos r' c') | r' <- take squareSize [(off r)..]
                                 , c' <- take squareSize [(off c)..]]
  where off x = x `div` squareSize * squareSize

-- TODO test squareVals

-- TODO capture overspecified fields (possible vals empty)

possibleVals :: Board -> Pos -> [Int]
possibleVals b pos = [1..boardSize] \\ concat [ext b pos | ext <- [rowVals, colVals, squareVals]]

seqSnd :: (a, Maybe b) -> Maybe (a, b)
seqSnd (a, Nothing) = Nothing
seqSnd (a, Just b) = Just (a, b)

allPossibleVals :: Board -> [(Pos, [Int])]
allPossibleVals b = catMaybes [seqSnd (pos, possVals b pos) | pos <- positions]
  where positions = [Pos r c | r <- take boardSize [0..], c <- take boardSize [0..]]
        possVals :: Board -> Pos -> Maybe [Int]
        possVals b pos = case (field b pos) of
                           Just _ -> Nothing
                           Nothing -> Just $ possibleVals b pos

bestPossibleVals :: Board -> Maybe (Pos, [Int])
bestPossibleVals b = listToMaybe $ sortByLength (allPossibleVals b)
  where sortByLength = sortBy (\ (_, avs) (_, bvs) -> compare (length avs) (length bvs))

update :: Board -> Pos -> Int -> Board
update (Board b) p v = Board $ DS.update i (Just v) b
  where i = posToIdx p

solve :: Board -> Maybe Board
solve b = step possVals
  where possVals = bestPossibleVals b
        step :: Maybe (Pos, [Int]) -> Maybe Board
        step Nothing = Just b
        step (Just (p, [])) = Nothing
        step (Just (p, vs)) = listToMaybe $ catMaybes [solve (update b p v) | v <- vs]

trySolve :: Board -> Board
trySolve b =
  case (solve b) of
    Nothing -> b
    Just sb -> sb
