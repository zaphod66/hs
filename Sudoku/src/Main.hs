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
import Sudoku

board1 = intsToBoard norm1
board2 = intsToBoard grid1
board3 = intsToBoard grid2
board4 = intsToBoard hard1
board5 = intsToBoard zero1

intsToBoard :: [Int] -> Board
intsToBoard b = Board $ map convert b
  where
    convert :: Int -> Field
    convert 0 = Nothing
    convert d = Just d

norm1 =
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

grid1 =
    [
        0,0,3,0,2,0,6,0,0,
        9,0,0,3,0,5,0,0,1,
        0,0,1,8,0,6,4,0,0,
        0,0,8,1,0,2,9,0,0,
        7,0,0,0,0,0,0,0,8,
        0,0,6,7,0,8,2,0,0,
        0,0,2,6,0,9,5,0,0,
        8,0,0,2,0,3,0,0,9,
        0,0,5,0,1,0,3,0,0
    ]

grid2 =
    [
        4,0,0,0,0,0,8,0,5,
        0,3,0,0,0,0,0,0,0,
        0,0,0,7,0,0,0,0,0,
        0,2,0,0,0,0,0,6,0,
        0,0,0,0,8,0,4,0,0,
        0,0,0,0,1,0,0,0,0,
        0,0,0,6,0,3,0,7,0,
        5,0,0,2,0,0,0,0,0,
        1,0,4,0,0,0,0,0,0
    ]

hard1 =
    [
        0,0,0,0,0,6,0,0,0,
        0,5,9,0,0,0,0,0,8,
        2,0,0,0,0,8,0,0,0,
        0,4,5,0,0,0,0,0,0,
        0,0,3,0,0,0,0,0,0,
        0,0,6,0,0,3,0,5,4,
        0,0,0,3,2,5,0,0,6,
        0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0
    ]

zero1 =
    [
        0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0
    ]

exeMain = do
    putStrLn (show board1)
    putStrLn ("==========")
    putStrLn (show $ solve board1)

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

