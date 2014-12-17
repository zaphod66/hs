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

board = Board $ map convert template
  where
    convert :: Int -> Field
    convert 0 = Nothing
    convert d = Just d
    template =
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

exeMain = do
    putStrLn (show board)

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

