{-# LANGUAGE CPP, TemplateHaskell #-}

module Sudoku where

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Gen
import Data.Maybe
import Data.List

runTests = $quickCheckAll

boardSize = 9
squareSize = 3

type Field = Maybe Int

-- TODO convert to Data.Sequence
type Board = [Field]

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
field b p = b !! (posToIdx p)

prop_field pos @ (Pos r c) =
  field board pos == (Just $ r * boardSize + c)
  where board = map (\i -> Just i) (take (boardSize ^ 2) [0..])

rowVals :: Board -> Pos -> [Int]
rowVals b (Pos r c) = catMaybes [field b (Pos r c') | c' <- take boardSize [0..]]

prop_rowVals pos @ (Pos r _) =
  sort rv == take boardSize [(r * boardSize)..]
  where board = map (\i -> Just i) (take (boardSize ^ 2) [0..])
        rv = rowVals board pos

colVals :: Board -> Pos -> [Int]
colVals b (Pos r c) = catMaybes [field b (Pos r' c) | r' <- take boardSize [0..]]

prop_colVals pos @ (Pos _ c) =
  sort cv == [v * boardSize + c | v <- take boardSize [0..]]
  where board = map (\i -> Just i) (take (boardSize ^ 2) [0..])
        cv = colVals board pos

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
update b p v = (take i b) ++ [Just v] ++ (drop (i + 1) b)
  where i = posToIdx p

solve :: Board -> Maybe Board
solve b = step possVals
  where possVals = bestPossibleVals b
        step :: Maybe (Pos, [Int]) -> Maybe Board
        step Nothing = Just b
        step (Just (p, [])) = Nothing
        step (Just (p, vs)) = listToMaybe $ catMaybes [solve (update b p v) | v <- vs]


