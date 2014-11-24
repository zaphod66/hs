{-# LANGUAGE CPP, TemplateHaskell #-}

module Sudoku where

import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Gen

runTests = $quickCheckAll

boardSize = 9

type Field = Maybe Int

type Board = [Field]

data Pos = Pos Int Int deriving Show

posGen :: Gen Pos
posGen =
  do
    x <- choose (0, boardSize - 1)
    y <- choose (0, boardSize - 1)
    return (Pos x y)

instance Arbitrary Pos where
  arbitrary = posGen

field :: Board -> Pos -> Field
field b (Pos x y) = b !! ((x * boardSize) + y)

prop_field pos @ (Pos x y) =
  field board pos == (Just $ x * boardSize + y + 1)
  where board = map (\i -> Just i) [1..81]
