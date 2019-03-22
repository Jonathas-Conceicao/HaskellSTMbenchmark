import CCHR.CCHRSolver
import GHC.Conc
import CCHR.Control.State

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

import List

import CCHR.CCHRCommandLine

data SCell = SCell Int Int deriving Ord

instance Eq SCell where
   (SCell i j) == (SCell i' j') = (i==i') && (j==j')

instance Show SCell where
   show (SCell i j) = "x(" ++ (show i) ++ "," ++ (show j) ++ ")"

instance Eq Sudoku where
   Dom(s,is) == Dom(s',is') = (s==s') && (is==is')
   _ == _ = False

instance Show Sudoku where
   show (Dom(x,is)) = "Dom(" ++ (show x) ++ "," ++ (show is) ++ ")"

index :: [[a]] -> Int -> Int -> a
index bs i j = (bs !! i) !! j

set :: [[a]] -> Int -> Int -> a -> [[a]]
set (as:ass) 0 j a = 
         (set' as j a):ass
         where
            set' :: [a] -> Int -> a -> [a]
            set' (a:as) 0 a' = a':as
            set' (a:as) i a' = a:(set' as (i-1) a')
            set' [] i a = []            
set (as:ass) i j a = as:(set ass (i-1) j a)
set [] i j a = [] 

printSudoku :: [[Int]] -> IO ()
printSudoku (as:ass) = do
     printSudoku' as
     --putStrLn ""
     printSudoku ass
     where
        printSudoku' :: [Int] -> IO ()
        printSudoku' (a:as) = do
             putStr ((show a) ++ " ")
             printSudoku' as
        printSudoku' [] = putStrLn ""
printSudoku [] = return ()

testSudoku :: [[Int]]
testSudoku = [
              [3,0,8,0,0,5,0,1,0],
              [2,0,0,9,0,0,3,7,4],
              [6,0,0,0,0,0,2,0,5],
              [5,0,0,8,0,3,0,0,9],
              [0,0,3,0,9,0,8,0,0],
              [7,0,0,5,0,4,0,0,3],
              [8,0,6,0,0,0,0,0,1],
              [1,4,5,0,0,8,0,0,2],
              [0,3,0,1,0,0,4,0,8]
             ]

emptySudoku :: [[Int]]
emptySudoku = [
               [0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0]
              ]

internalize :: [[Int]] -> [Sudoku]
internalize ss = let s1 = internalize' (ss !! 0) 0 0
                     s2 = internalize' (ss !! 1) 1 0
                     s3 = internalize' (ss !! 2) 2 0
                     s4 = internalize' (ss !! 3) 3 0
                     s5 = internalize' (ss !! 4) 4 0
                     s6 = internalize' (ss !! 5) 5 0
                     s7 = internalize' (ss !! 6) 6 0
                     s8 = internalize' (ss !! 7) 7 0
                     s9 = internalize' (ss !! 8) 8 0
                 in s1 ++ s2 ++ s3 ++ s4 ++ s5 ++ s6 ++ s7 ++ s8 ++ s9
                 where
                    internalize' :: [Int] -> Int -> Int -> [Sudoku]
                    internalize' (x:xs) i j = case x of
                                                0 -> (Dom(SCell i j,[1..9])):(internalize' xs i (j+1))
                                                _ -> (Dom(SCell i j,[x])):(internalize' xs i (j+1))
                    internalize' [] _ _ = []

sameBlock :: (Int,Int) -> (Int,Int) -> Bool
sameBlock (i,j) (k,l) = 
       case (i==k)&&(j==l) of
          True  -> False
          False -> (sameBlock' i k) && (sameBlock' j l)
       where
          sameBlock' :: Int -> Int -> Bool
          sameBlock' i j = let di = i `div` 3
                               dj = j `div` 3
                           in (di == dj)

externalize :: [Sudoku] -> [[Int]]
externalize ss = externalize' emptySudoku ss
                 where
                   externalize' :: [[Int]] -> [Sudoku] -> [[Int]]
                   externalize' exs (Dom(SCell i j,[x]):ss) = externalize' (set exs i j x) ss
                   externalize' exs (_:ss) = externalize' exs ss
                   externalize' exs [] = exs

main :: IO ()
main = do
    putStrLn "Sudoku Problem:"
    printSudoku testSudoku  
    putStrLn ""  
    runCommand (internalize testSudoku)
    -- readTStats

data Sudoku = Dom(SCell,[Int]) | Try(Int) deriving Ord

data R1 = R1 R1Pat | R1Dummy deriving (Show,Eq,Ord)
data R1Pat = P1 | P2 deriving (Show,Eq,Ord) 

instance CHRRule R1 where
   allPatterns = [R1 P1,R1 P2]

data R2 = R2 R2Pat | R2Dummy deriving (Show,Eq,Ord)
data R2Pat = P3 | P4 deriving (Show,Eq,Ord) 

instance CHRRule R2 where
   allPatterns = [R2 P3,R2 P4]

data R3 = R3 R3Pat | R3Dummy deriving (Show,Eq,Ord)
data R3Pat = P5 | P6 deriving (Show,Eq,Ord) 

instance CHRRule R3 where
   allPatterns = [R3 P5,R3 P6]

data R4 = R4 R4Pat | R4Dummy deriving (Show,Eq,Ord)
data R4Pat = P7 deriving (Show,Eq,Ord) 

instance CHRRule R4 where
   allPatterns = [R4 P7]

instance CHROperations R1 Sudoku (Maybe Int,Maybe Int,Maybe Int,Maybe Int,Maybe [Int]) where
   matchPattern (R1 P1) (Just i',Just j',Just x',mb4,mb5) (Dom((SCell i j),[x])) | (i==i')&&(j==j')&&(x==x') = Just (Just i,Just j,Just x,mb4,mb5)
   matchPattern (R1 P1) (Just i',Just j',Nothing,mb4,mb5) (Dom((SCell i j),[x])) | (i==i')&&(j==j') = Just (Just i,Just j,Just x,mb4,mb5)
   matchPattern (R1 P1) (Just i',Nothing,Just x',mb4,mb5) (Dom((SCell i j),[x])) | (i==i')&&(x==x') = Just (Just i,Just j,Just x,mb4,mb5)
   matchPattern (R1 P1) (Just i',Nothing,Nothing,mb4,mb5) (Dom((SCell i j),[x])) | (i==i') = Just (Just i,Just j,Just x,mb4,mb5)
   matchPattern (R1 P1) (Nothing,Just j',Just x',mb4,mb5) (Dom((SCell i j),[x])) | (j==j')&&(x==x') = Just (Just i,Just j,Just x,mb4,mb5)
   matchPattern (R1 P1) (Nothing,Just j',Nothing,mb4,mb5) (Dom((SCell i j),[x])) | (j==j') = Just (Just i,Just j,Just x,mb4,mb5)
   matchPattern (R1 P1) (Nothing,Nothing,Just x',mb4,mb5) (Dom((SCell i j),[x])) | (x==x') = Just (Just i,Just j,Just x,mb4,mb5)
   matchPattern (R1 P1) (Nothing,Nothing,Nothing,mb4,mb5) (Dom((SCell i j),[x])) = Just (Just i,Just j,Just x,mb4,mb5)
   matchPattern (R1 P1) _ _ = Nothing 

   matchPattern (R1 P2) (Just i',mb2,mb3,Just k',Just ys') (Dom((SCell i k),ys)) | (i==i')&&(k==k')&&(ys==ys') = Just (Just i,mb2,mb3,Just k,Just ys)
   matchPattern (R1 P2) (Just i',mb2,mb3,Just k',Nothing) (Dom((SCell i k),ys)) | (i==i')&&(k==k') = Just (Just i,mb2,mb3,Just k,Just ys)
   matchPattern (R1 P2) (Just i',mb2,mb3,Nothing,Just ys') (Dom((SCell i k),ys)) | (i==i')&&(ys==ys') = Just (Just i,mb2,mb3,Just k,Just ys)
   matchPattern (R1 P2) (Just i',mb2,mb3,Nothing,Nothing) (Dom((SCell i k),ys)) | (i==i') = Just (Just i,mb2,mb3,Just k,Just ys)
   matchPattern (R1 P2) (Nothing,mb2,mb3,Just k',Just ys') (Dom((SCell i k),ys)) | (k==k')&&(ys==ys') = Just (Just i,mb2,mb3,Just k,Just ys)
   matchPattern (R1 P2) (Nothing,mb2,mb3,Just k',Nothing) (Dom((SCell i k),ys)) | (k==k') = Just (Just i,mb2,mb3,Just k,Just ys)
   matchPattern (R1 P2) (Nothing,mb2,mb3,Nothing,Just ys') (Dom((SCell i k),ys)) | (ys==ys') = Just (Just i,mb2,mb3,Just k,Just ys)
   matchPattern (R1 P2) (Nothing,mb2,mb3,Nothing,Nothing) (Dom((SCell i k),ys)) = Just (Just i,mb2,mb3,Just k,Just ys)
   matchPattern (R1 P2) _ _ = Nothing 

   checkGuard _ (Just i,Just j,Just x,Just k,Just ys) = (j /= k) && (elem x ys) && (ys /= []) 
   checkGuard _ _ = False 

   instBody _ (Just i,Just j,Just x,Just k,Just ys) = Just [Dom(SCell i j,[x]),Dom(SCell i k,delete x ys)]
   instBody _ _ = Just [] 


instance CHROperations R2 Sudoku (Maybe SCell,Maybe Int,Maybe Int,Maybe Int,Maybe SCell,Maybe Int,Maybe [Int]) where
   matchPattern (R2 P3) (Just c1',Just j',Just i',Just x',mb5,mb6,mb7) (Dom(c1@((SCell j i)),[x])) | (c1==c1')&&(j==j')&&(i==i')&&(x==x') = Just (Just c1,Just j,Just i,Just x,mb5,mb6,mb7)
   matchPattern (R2 P3) (Just c1',Just j',Just i',Nothing,mb5,mb6,mb7) (Dom(c1@((SCell j i)),[x])) | (c1==c1')&&(j==j')&&(i==i') = Just (Just c1,Just j,Just i,Just x,mb5,mb6,mb7)
   matchPattern (R2 P3) (Just c1',Just j',Nothing,Just x',mb5,mb6,mb7) (Dom(c1@((SCell j i)),[x])) | (c1==c1')&&(j==j')&&(x==x') = Just (Just c1,Just j,Just i,Just x,mb5,mb6,mb7)
   matchPattern (R2 P3) (Just c1',Just j',Nothing,Nothing,mb5,mb6,mb7) (Dom(c1@((SCell j i)),[x])) | (c1==c1')&&(j==j') = Just (Just c1,Just j,Just i,Just x,mb5,mb6,mb7)
   matchPattern (R2 P3) (Just c1',Nothing,Just i',Just x',mb5,mb6,mb7) (Dom(c1@((SCell j i)),[x])) | (c1==c1')&&(i==i')&&(x==x') = Just (Just c1,Just j,Just i,Just x,mb5,mb6,mb7)
   matchPattern (R2 P3) (Just c1',Nothing,Just i',Nothing,mb5,mb6,mb7) (Dom(c1@((SCell j i)),[x])) | (c1==c1')&&(i==i') = Just (Just c1,Just j,Just i,Just x,mb5,mb6,mb7)
   matchPattern (R2 P3) (Just c1',Nothing,Nothing,Just x',mb5,mb6,mb7) (Dom(c1@((SCell j i)),[x])) | (c1==c1')&&(x==x') = Just (Just c1,Just j,Just i,Just x,mb5,mb6,mb7)
   matchPattern (R2 P3) (Just c1',Nothing,Nothing,Nothing,mb5,mb6,mb7) (Dom(c1@((SCell j i)),[x])) | (c1==c1') = Just (Just c1,Just j,Just i,Just x,mb5,mb6,mb7)
   matchPattern (R2 P3) (Nothing,Just j',Just i',Just x',mb5,mb6,mb7) (Dom(c1@((SCell j i)),[x])) | (j==j')&&(i==i')&&(x==x') = Just (Just c1,Just j,Just i,Just x,mb5,mb6,mb7)
   matchPattern (R2 P3) (Nothing,Just j',Just i',Nothing,mb5,mb6,mb7) (Dom(c1@((SCell j i)),[x])) | (j==j')&&(i==i') = Just (Just c1,Just j,Just i,Just x,mb5,mb6,mb7)
   matchPattern (R2 P3) (Nothing,Just j',Nothing,Just x',mb5,mb6,mb7) (Dom(c1@((SCell j i)),[x])) | (j==j')&&(x==x') = Just (Just c1,Just j,Just i,Just x,mb5,mb6,mb7)
   matchPattern (R2 P3) (Nothing,Just j',Nothing,Nothing,mb5,mb6,mb7) (Dom(c1@((SCell j i)),[x])) | (j==j') = Just (Just c1,Just j,Just i,Just x,mb5,mb6,mb7)
   matchPattern (R2 P3) (Nothing,Nothing,Just i',Just x',mb5,mb6,mb7) (Dom(c1@((SCell j i)),[x])) | (i==i')&&(x==x') = Just (Just c1,Just j,Just i,Just x,mb5,mb6,mb7)
   matchPattern (R2 P3) (Nothing,Nothing,Just i',Nothing,mb5,mb6,mb7) (Dom(c1@((SCell j i)),[x])) | (i==i') = Just (Just c1,Just j,Just i,Just x,mb5,mb6,mb7)
   matchPattern (R2 P3) (Nothing,Nothing,Nothing,Just x',mb5,mb6,mb7) (Dom(c1@((SCell j i)),[x])) | (x==x') = Just (Just c1,Just j,Just i,Just x,mb5,mb6,mb7)
   matchPattern (R2 P3) (Nothing,Nothing,Nothing,Nothing,mb5,mb6,mb7) (Dom(c1@((SCell j i)),[x])) = Just (Just c1,Just j,Just i,Just x,mb5,mb6,mb7)
   matchPattern (R2 P3) _ _ = Nothing 

   matchPattern (R2 P4) (mb1,mb2,Just i',mb4,Just c2',Just k',Just ys') (Dom(c2@((SCell k i)),ys)) | (i==i')&&(c2==c2')&&(k==k')&&(ys==ys') = Just (mb1,mb2,Just i,mb4,Just c2,Just k,Just ys)
   matchPattern (R2 P4) (mb1,mb2,Just i',mb4,Just c2',Just k',Nothing) (Dom(c2@((SCell k i)),ys)) | (i==i')&&(c2==c2')&&(k==k') = Just (mb1,mb2,Just i,mb4,Just c2,Just k,Just ys)
   matchPattern (R2 P4) (mb1,mb2,Just i',mb4,Just c2',Nothing,Just ys') (Dom(c2@((SCell k i)),ys)) | (i==i')&&(c2==c2')&&(ys==ys') = Just (mb1,mb2,Just i,mb4,Just c2,Just k,Just ys)
   matchPattern (R2 P4) (mb1,mb2,Just i',mb4,Just c2',Nothing,Nothing) (Dom(c2@((SCell k i)),ys)) | (i==i')&&(c2==c2') = Just (mb1,mb2,Just i,mb4,Just c2,Just k,Just ys)
   matchPattern (R2 P4) (mb1,mb2,Just i',mb4,Nothing,Just k',Just ys') (Dom(c2@((SCell k i)),ys)) | (i==i')&&(k==k')&&(ys==ys') = Just (mb1,mb2,Just i,mb4,Just c2,Just k,Just ys)
   matchPattern (R2 P4) (mb1,mb2,Just i',mb4,Nothing,Just k',Nothing) (Dom(c2@((SCell k i)),ys)) | (i==i')&&(k==k') = Just (mb1,mb2,Just i,mb4,Just c2,Just k,Just ys)
   matchPattern (R2 P4) (mb1,mb2,Just i',mb4,Nothing,Nothing,Just ys') (Dom(c2@((SCell k i)),ys)) | (i==i')&&(ys==ys') = Just (mb1,mb2,Just i,mb4,Just c2,Just k,Just ys)
   matchPattern (R2 P4) (mb1,mb2,Just i',mb4,Nothing,Nothing,Nothing) (Dom(c2@((SCell k i)),ys)) | (i==i') = Just (mb1,mb2,Just i,mb4,Just c2,Just k,Just ys)
   matchPattern (R2 P4) (mb1,mb2,Nothing,mb4,Just c2',Just k',Just ys') (Dom(c2@((SCell k i)),ys)) | (c2==c2')&&(k==k')&&(ys==ys') = Just (mb1,mb2,Just i,mb4,Just c2,Just k,Just ys)
   matchPattern (R2 P4) (mb1,mb2,Nothing,mb4,Just c2',Just k',Nothing) (Dom(c2@((SCell k i)),ys)) | (c2==c2')&&(k==k') = Just (mb1,mb2,Just i,mb4,Just c2,Just k,Just ys)
   matchPattern (R2 P4) (mb1,mb2,Nothing,mb4,Just c2',Nothing,Just ys') (Dom(c2@((SCell k i)),ys)) | (c2==c2')&&(ys==ys') = Just (mb1,mb2,Just i,mb4,Just c2,Just k,Just ys)
   matchPattern (R2 P4) (mb1,mb2,Nothing,mb4,Just c2',Nothing,Nothing) (Dom(c2@((SCell k i)),ys)) | (c2==c2') = Just (mb1,mb2,Just i,mb4,Just c2,Just k,Just ys)
   matchPattern (R2 P4) (mb1,mb2,Nothing,mb4,Nothing,Just k',Just ys') (Dom(c2@((SCell k i)),ys)) | (k==k')&&(ys==ys') = Just (mb1,mb2,Just i,mb4,Just c2,Just k,Just ys)
   matchPattern (R2 P4) (mb1,mb2,Nothing,mb4,Nothing,Just k',Nothing) (Dom(c2@((SCell k i)),ys)) | (k==k') = Just (mb1,mb2,Just i,mb4,Just c2,Just k,Just ys)
   matchPattern (R2 P4) (mb1,mb2,Nothing,mb4,Nothing,Nothing,Just ys') (Dom(c2@((SCell k i)),ys)) | (ys==ys') = Just (mb1,mb2,Just i,mb4,Just c2,Just k,Just ys)
   matchPattern (R2 P4) (mb1,mb2,Nothing,mb4,Nothing,Nothing,Nothing) (Dom(c2@((SCell k i)),ys)) = Just (mb1,mb2,Just i,mb4,Just c2,Just k,Just ys)
   matchPattern (R2 P4) _ _ = Nothing 

   checkGuard _ (Just c1,Just j,Just i,Just x,Just c2,Just k,Just ys) = (j /= k) && (elem x ys) && (ys /= []) 
   checkGuard _ _ = False 

   instBody _ (Just c1,Just j,Just i,Just x,Just c2,Just k,Just ys) = Just [Dom(c1,[x]),Dom(c2,delete x ys)]
   instBody _ _ = Just [] 


instance CHROperations R3 Sudoku (Maybe SCell,Maybe Int,Maybe Int,Maybe Int,Maybe SCell,Maybe Int,Maybe Int,Maybe [Int]) where
   matchPattern (R3 P5) (Just c1',Just i',Just j',Just x',mb5,mb6,mb7,mb8) (Dom(c1@((SCell i j)),[x])) | (c1==c1')&&(i==i')&&(j==j')&&(x==x') = Just (Just c1,Just i,Just j,Just x,mb5,mb6,mb7,mb8)
   matchPattern (R3 P5) (Just c1',Just i',Just j',Nothing,mb5,mb6,mb7,mb8) (Dom(c1@((SCell i j)),[x])) | (c1==c1')&&(i==i')&&(j==j') = Just (Just c1,Just i,Just j,Just x,mb5,mb6,mb7,mb8)
   matchPattern (R3 P5) (Just c1',Just i',Nothing,Just x',mb5,mb6,mb7,mb8) (Dom(c1@((SCell i j)),[x])) | (c1==c1')&&(i==i')&&(x==x') = Just (Just c1,Just i,Just j,Just x,mb5,mb6,mb7,mb8)
   matchPattern (R3 P5) (Just c1',Just i',Nothing,Nothing,mb5,mb6,mb7,mb8) (Dom(c1@((SCell i j)),[x])) | (c1==c1')&&(i==i') = Just (Just c1,Just i,Just j,Just x,mb5,mb6,mb7,mb8)
   matchPattern (R3 P5) (Just c1',Nothing,Just j',Just x',mb5,mb6,mb7,mb8) (Dom(c1@((SCell i j)),[x])) | (c1==c1')&&(j==j')&&(x==x') = Just (Just c1,Just i,Just j,Just x,mb5,mb6,mb7,mb8)
   matchPattern (R3 P5) (Just c1',Nothing,Just j',Nothing,mb5,mb6,mb7,mb8) (Dom(c1@((SCell i j)),[x])) | (c1==c1')&&(j==j') = Just (Just c1,Just i,Just j,Just x,mb5,mb6,mb7,mb8)
   matchPattern (R3 P5) (Just c1',Nothing,Nothing,Just x',mb5,mb6,mb7,mb8) (Dom(c1@((SCell i j)),[x])) | (c1==c1')&&(x==x') = Just (Just c1,Just i,Just j,Just x,mb5,mb6,mb7,mb8)
   matchPattern (R3 P5) (Just c1',Nothing,Nothing,Nothing,mb5,mb6,mb7,mb8) (Dom(c1@((SCell i j)),[x])) | (c1==c1') = Just (Just c1,Just i,Just j,Just x,mb5,mb6,mb7,mb8)
   matchPattern (R3 P5) (Nothing,Just i',Just j',Just x',mb5,mb6,mb7,mb8) (Dom(c1@((SCell i j)),[x])) | (i==i')&&(j==j')&&(x==x') = Just (Just c1,Just i,Just j,Just x,mb5,mb6,mb7,mb8)
   matchPattern (R3 P5) (Nothing,Just i',Just j',Nothing,mb5,mb6,mb7,mb8) (Dom(c1@((SCell i j)),[x])) | (i==i')&&(j==j') = Just (Just c1,Just i,Just j,Just x,mb5,mb6,mb7,mb8)
   matchPattern (R3 P5) (Nothing,Just i',Nothing,Just x',mb5,mb6,mb7,mb8) (Dom(c1@((SCell i j)),[x])) | (i==i')&&(x==x') = Just (Just c1,Just i,Just j,Just x,mb5,mb6,mb7,mb8)
   matchPattern (R3 P5) (Nothing,Just i',Nothing,Nothing,mb5,mb6,mb7,mb8) (Dom(c1@((SCell i j)),[x])) | (i==i') = Just (Just c1,Just i,Just j,Just x,mb5,mb6,mb7,mb8)
   matchPattern (R3 P5) (Nothing,Nothing,Just j',Just x',mb5,mb6,mb7,mb8) (Dom(c1@((SCell i j)),[x])) | (j==j')&&(x==x') = Just (Just c1,Just i,Just j,Just x,mb5,mb6,mb7,mb8)
   matchPattern (R3 P5) (Nothing,Nothing,Just j',Nothing,mb5,mb6,mb7,mb8) (Dom(c1@((SCell i j)),[x])) | (j==j') = Just (Just c1,Just i,Just j,Just x,mb5,mb6,mb7,mb8)
   matchPattern (R3 P5) (Nothing,Nothing,Nothing,Just x',mb5,mb6,mb7,mb8) (Dom(c1@((SCell i j)),[x])) | (x==x') = Just (Just c1,Just i,Just j,Just x,mb5,mb6,mb7,mb8)
   matchPattern (R3 P5) (Nothing,Nothing,Nothing,Nothing,mb5,mb6,mb7,mb8) (Dom(c1@((SCell i j)),[x])) = Just (Just c1,Just i,Just j,Just x,mb5,mb6,mb7,mb8)
   matchPattern (R3 P5) _ _ = Nothing 

   matchPattern (R3 P6) (mb1,mb2,mb3,mb4,Just c2',Just k',Just l',Just ys') (Dom(c2@((SCell k l)),ys)) | (c2==c2')&&(k==k')&&(l==l')&&(ys==ys') = Just (mb1,mb2,mb3,mb4,Just c2,Just k,Just l,Just ys)
   matchPattern (R3 P6) (mb1,mb2,mb3,mb4,Just c2',Just k',Just l',Nothing) (Dom(c2@((SCell k l)),ys)) | (c2==c2')&&(k==k')&&(l==l') = Just (mb1,mb2,mb3,mb4,Just c2,Just k,Just l,Just ys)
   matchPattern (R3 P6) (mb1,mb2,mb3,mb4,Just c2',Just k',Nothing,Just ys') (Dom(c2@((SCell k l)),ys)) | (c2==c2')&&(k==k')&&(ys==ys') = Just (mb1,mb2,mb3,mb4,Just c2,Just k,Just l,Just ys)
   matchPattern (R3 P6) (mb1,mb2,mb3,mb4,Just c2',Just k',Nothing,Nothing) (Dom(c2@((SCell k l)),ys)) | (c2==c2')&&(k==k') = Just (mb1,mb2,mb3,mb4,Just c2,Just k,Just l,Just ys)
   matchPattern (R3 P6) (mb1,mb2,mb3,mb4,Just c2',Nothing,Just l',Just ys') (Dom(c2@((SCell k l)),ys)) | (c2==c2')&&(l==l')&&(ys==ys') = Just (mb1,mb2,mb3,mb4,Just c2,Just k,Just l,Just ys)
   matchPattern (R3 P6) (mb1,mb2,mb3,mb4,Just c2',Nothing,Just l',Nothing) (Dom(c2@((SCell k l)),ys)) | (c2==c2')&&(l==l') = Just (mb1,mb2,mb3,mb4,Just c2,Just k,Just l,Just ys)
   matchPattern (R3 P6) (mb1,mb2,mb3,mb4,Just c2',Nothing,Nothing,Just ys') (Dom(c2@((SCell k l)),ys)) | (c2==c2')&&(ys==ys') = Just (mb1,mb2,mb3,mb4,Just c2,Just k,Just l,Just ys)
   matchPattern (R3 P6) (mb1,mb2,mb3,mb4,Just c2',Nothing,Nothing,Nothing) (Dom(c2@((SCell k l)),ys)) | (c2==c2') = Just (mb1,mb2,mb3,mb4,Just c2,Just k,Just l,Just ys)
   matchPattern (R3 P6) (mb1,mb2,mb3,mb4,Nothing,Just k',Just l',Just ys') (Dom(c2@((SCell k l)),ys)) | (k==k')&&(l==l')&&(ys==ys') = Just (mb1,mb2,mb3,mb4,Just c2,Just k,Just l,Just ys)
   matchPattern (R3 P6) (mb1,mb2,mb3,mb4,Nothing,Just k',Just l',Nothing) (Dom(c2@((SCell k l)),ys)) | (k==k')&&(l==l') = Just (mb1,mb2,mb3,mb4,Just c2,Just k,Just l,Just ys)
   matchPattern (R3 P6) (mb1,mb2,mb3,mb4,Nothing,Just k',Nothing,Just ys') (Dom(c2@((SCell k l)),ys)) | (k==k')&&(ys==ys') = Just (mb1,mb2,mb3,mb4,Just c2,Just k,Just l,Just ys)
   matchPattern (R3 P6) (mb1,mb2,mb3,mb4,Nothing,Just k',Nothing,Nothing) (Dom(c2@((SCell k l)),ys)) | (k==k') = Just (mb1,mb2,mb3,mb4,Just c2,Just k,Just l,Just ys)
   matchPattern (R3 P6) (mb1,mb2,mb3,mb4,Nothing,Nothing,Just l',Just ys') (Dom(c2@((SCell k l)),ys)) | (l==l')&&(ys==ys') = Just (mb1,mb2,mb3,mb4,Just c2,Just k,Just l,Just ys)
   matchPattern (R3 P6) (mb1,mb2,mb3,mb4,Nothing,Nothing,Just l',Nothing) (Dom(c2@((SCell k l)),ys)) | (l==l') = Just (mb1,mb2,mb3,mb4,Just c2,Just k,Just l,Just ys)
   matchPattern (R3 P6) (mb1,mb2,mb3,mb4,Nothing,Nothing,Nothing,Just ys') (Dom(c2@((SCell k l)),ys)) | (ys==ys') = Just (mb1,mb2,mb3,mb4,Just c2,Just k,Just l,Just ys)
   matchPattern (R3 P6) (mb1,mb2,mb3,mb4,Nothing,Nothing,Nothing,Nothing) (Dom(c2@((SCell k l)),ys)) = Just (mb1,mb2,mb3,mb4,Just c2,Just k,Just l,Just ys)
   matchPattern (R3 P6) _ _ = Nothing 

   checkGuard _ (Just c1,Just i,Just j,Just x,Just c2,Just k,Just l,Just ys) = (elem x ys) && (ys /= []) && (sameBlock (i,j) (k,l)) 
   checkGuard _ _ = False 

   instBody _ (Just c1,Just i,Just j,Just x,Just c2,Just k,Just l,Just ys) = Just [Dom(c1,[x]),Dom(c2,delete x ys)]
   instBody _ _ = Just [] 


instance CHROperations R4 Sudoku () where
   matchPattern (R4 P7) () (Dom(_,[])) = Just ()
   matchPattern (R4 P7) _ _ = Nothing 

   checkGuard _ () = True 
   checkGuard _ _ = False 

   instBody _ () = Nothing 
   instBody _ _ = Nothing 


instance Constraint Sudoku where 
   derivation _ _ termSig _ [] = doAtomic (writeTVar termSig True)
   derivation (t,i0) sts termSig errSig (ic:ics) = do
            dontDrop <- doAtomic (validateContent ic)
            (if dontDrop then do
               (i1,ics1,b1) <- solverThread (t,i0) R1Dummy sts errSig ic False
               (i2,ics2,b2) <- solverThread (t,i1) R2Dummy sts errSig ic b1
               (i3,ics3,b3) <- solverThread (t,i2) R3Dummy sts errSig ic b2
               (i4,ics4,b4) <- solverThread (t,i3) R4Dummy sts errSig ic b3
               let ics' = ics1 ++ ics2 ++ ics3 ++ ics4
               derivation (t,i4) sts termSig errSig (ics ++ ics')
             else derivation (t,i0) sts termSig errSig ics)
