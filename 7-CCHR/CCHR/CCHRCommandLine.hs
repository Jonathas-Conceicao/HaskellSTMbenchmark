--------------------------------------------------------------------------------
--
-- Copyright (C) 2006 
--
-- This program is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the Free
-- Software Foundation; either version 2 of the License, or (at your option)
-- any later version. This program is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
--
--------------------------------------------------------------------------------

module CCHR.CCHRCommandLine where

import CCHR.CCHRSolver

import Data.Char
import Data.List
import System.IO
import System.Environment
import System.Time

--------------------------------------------------------------
-- Auxilliary Functions
--------------------------------------------------------------

mixIt :: Bool -> [a] -> [a]
mixIt _ [] = []
mixIt _ [a] = [a]
mixIt True (a:b:cs)  = [a] ++ (mixIt False cs) ++ [b]
mixIt False (a:b:cs) = [b] ++ (mixIt True cs) ++ [a]

pWrite :: FilePath -> String -> IO ()
pWrite file str = do
        putStrLn str
        appendFile (file ++ ".txt") (str ++ "\n")

pwSpace :: FilePath -> IO ()
pwSpace file = pWrite file "" 

--------------------------------------------------------------
-- Run and Record Test
--------------------------------------------------------------

rrTestSolver :: Constraint cons => FilePath -> [cons] -> Int -> Bool -> (Bool,Bool) -> IO [cons]
rrTestSolver file cons i b flags@(small,dist) = do
    pwSpace file
    let step  = if small then "SMALL" else "BIG"
        entry = if dist  then "DISTINCT" else "SAME"
    pWrite file ("Running Solver with " ++ step ++ " step search, " ++ entry ++ " entry points " ++ 
                 "and " ++ (show i) ++ " threads :")
    ck1 <- getClockTime
    cs <- (runSolver (dSolve cons i flags))
    ck2  <- getClockTime
    pWrite file ("Time Taken: " ++ (show (diffClockTimes ck2 ck1)))
    case b of
       False -> do pwSpace file
                   return cs
       True  -> do pWrite file (show cs)
                   pwSpace file
                   return cs

rrTestSuite :: Constraint cons => FilePath -> [cons] -> Bool -> Int -> (Bool,Bool,Bool,Bool) -> IO [cons]
rrTestSuite file cons b i (af,bf,cf,solo) = do
    pWrite file ("----------------------------------------------------------------------------------")
    pWrite file ("Test " ++ (show i) ++ " Begin: ")
    case solo of
      True  -> rrTestSolver file cons 1 b (True,True)
      False -> return []
    case af of
      True  -> do rrTestSolver file cons 2 b (False,False)
                  rrTestSolver file cons 8 b (False,False)
                  rrTestSolver file cons 16 b (False,False)
      False -> return []
    case bf of
      True  -> do rrTestSolver file cons 2 b (True,False)
                  rrTestSolver file cons 8 b (True,False)
                  rrTestSolver file cons 16 b (True,False)
      False -> return []
    case cf of
      True  -> do rrTestSolver file cons 2 b (True,True)
                  rrTestSolver file cons 8 b (True,True)
                  rrTestSolver file cons 16 b (True,True)
      False -> return []

beginTest :: Constraint cons => FilePath -> [cons] -> Bool -> (Bool,Bool,Bool,Bool) -> IO ()
beginTest file cons b flags = do
    pWrite file ("---------------------------------------------------------------------------------")
    pWrite file ("Multicore Solver Test: ")
    rrTestSuite file cons b 1 flags
    rrTestSuite file (reverse cons) b 2 flags
    let cons'   = mixIt True cons
    rrTestSuite file cons' b 3 flags
    return ()

strToInt :: [Char] -> Int
strToInt cs = let cs' = reverse cs
              in strToInt cs' 1
              where
                 strToInt (c:cs) i = ((digitToInt c)*i) + (strToInt cs (i*10))
                 strToInt [] _ = 0

threadOption :: [String] -> Int
threadOption (f:fs) = let (t,d) = span isAlpha f
                      in case (t=="t")&&(and (map isDigit d)) of
                           True  -> strToInt d
                           False -> threadOption fs
threadOption [] = 2

--------------------------------------------------------------
-- Top Level Interfaces
--------------------------------------------------------------

runCommand :: Constraint cons => [cons] -> IO ()
runCommand cs = do
    args <- getArgs
    case args of
      [] -> putStrLn "Usage: prog <Solver-Options>* <Test-Options>* <GHC-Options>*"
      fs -> do let i = threadOption fs
               case (elem "testSuite" fs) of
                 False -> do let small = if (elem "bigSteps" fs) then False else True
                                 dist  = if (elem "sameEntry" fs) then False else True 
                                 prt = if (elem "noPrint" fs) then False else True
                             writeFile ("outputRun.txt") ("Begin Solver Run: \n")
                             cs' <- rrTestSolver "outputRun" cs i prt (small,dist)
                             putStrLn "Results: \n"
                             putStrLn (show cs')
                 True  -> testOption cs

testOption :: Constraint cons => [cons] -> IO ()
testOption cs = do
    args <- getArgs
    case args of
      [] -> return ()
      fs -> do let b  = not (elem "noPrint" fs)
                   af = elem "A" fs
                   bf = elem "B" fs
                   cf = elem "C" fs
                   sf = elem "solo" fs
               writeFile "outputTestSuite.txt" "Begin Solver Test \n"
               beginTest "outputTestSuite" cs b (af,bf,cf,sf)
    
