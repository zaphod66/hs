{-# LANGUAGE CPP, TemplateHaskell #-}

module Sudoku where

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Gen
import Data.Maybe

runTests = $quickCheckAll

boardSize = 9
squareSize = 3

type Field = Maybe Int

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

field :: Board -> Pos -> Field
field b (Pos r c) = b !! ((r * boardSize) + c)

prop_field pos @ (Pos r c) =
  field board pos == (Just $ r * boardSize + c + 1)
  where board = map (\i -> Just i) [1..81]

rowVals :: Board -> Pos -> [Int]
rowVals b (Pos r c) = catMaybes [field b (Pos r c') | c' <- take boardSize [0..]]

colVals :: Board -> Pos -> [Int]
colVals b (Pos r c) = catMaybes [field b (Pos r' c) | r' <- take boardSize [0..]]

squareVals :: Board -> Pos -> [Int]
squareVals b (Pos r c) =
  catMaybes [field b (Pos r' c') | r' <- take squareSize [(off r)..]
                                 , c' <- take squareSize [(off c)..]]
  where off x = x `div` squareSize * squareSize



-- free :: Board -> [Pos]

-- update :: Board -> Pos -> Int -> Board
