{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (unless)
import Data.List (stripPrefix)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import Data.Char

import Sudoku

board = intsToBoard grid0

toBoard :: (a -> Field) -> [a] -> Board
toBoard f cs = Board $ map f cs

intsToBoard :: [Int] -> Board
intsToBoard  b = toBoard f b
  where
    f :: Int -> Field
    f 0 = Nothing
    f d = Just d

charsToBoard :: [Char] -> Board
charsToBoard b = toBoard f b
  where
    f :: Char -> Field
    f '.' = Nothing
    f '0' = Nothing
    f  c  = Just ((ord c) - (ord '0'))

grid0 =
    [
        0,0,7,0,2,1,0,6,0,
        0,8,0,0,0,0,0,0,0,
        0,0,0,6,5,0,0,0,4,
        6,9,0,3,0,8,2,0,0,
        0,0,0,0,0,0,5,0,0,
        3,2,0,5,0,9,7,0,0,
        0,0,0,1,8,0,0,0,5,
        0,7,0,0,0,0,0,0,0,
        0,0,3,0,6,2,0,9,0
    ]

zeros   = "................................................................................."
grid1   = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
grid2   = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
hard1   = ".....6....59.....82....8....45........3........6..3.54...325..6.................."
-- from http://norvig.com/top95.txt
top95_1 = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
top95_2 = "52...6.........7.13...........4..8..6......5...........418.........3..2...87....."
top95_3 = "6.....8.3.4.7.................5.4.7.3..2.....1.6.......2.....5.....8.6......1...."
-- from http://norvig.com/hardest.txt
hardest1 = "85...24..72......9..4.........1.7..23.5...9...4...........8..7..17..........36.4."
hardest2 = "..53.....8......2..7..1.5..4....53...1..7...6..32...8..6.5....9..4....3......97.."

exeMain = do
    putStrLn $ show board
    putStrLn ("===========")
    putStrLn $ show $ trySolve board

testMain = do
    allPass <- runTests
    unless allPass exitFailure

-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION

