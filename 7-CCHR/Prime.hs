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

import System.IO
import System.Environment
import System.Time

import CCHR.CCHRCommandLine

instance Eq Prime where
   Prime(i) == Prime(j) = i == j
   Candidate(i) == Candidate(j) = i == j
   _ == _ = False


instance Show Prime where
   show (Prime(i))     = "Prime(" ++ (show i) ++ ")"
   show (Candidate(i)) = "candidate(" ++ (show i) ++ ")"

parallelPrime :: Int -> [Prime]
parallelPrime n | n<2 = []
parallelPrime n | (n `mod` 2 == 0) = Prime(n):(parallelPrime (n-1))
parallelPrime n | (n `mod` 2 == 1) = (parallelPrime (n-1)) ++ [Prime(n)]

singlePrime :: Int -> [Prime]
singlePrime n = [Candidate(n)]

main :: IO ()
main = do
   let pr = parallelPrime 4000
   -- let pr = parallelPrime 2000
   runCommand pr
   -- readTStats

data Prime = Prime(Int) | Candidate(Int) deriving Ord

data R1 = R1 R1Pat | R1Dummy deriving (Show,Eq,Ord)
data R1Pat = P1 deriving (Show,Eq,Ord) 

instance CHRRule R1 where
   allPatterns = [R1 P1]

data R2 = R2 R2Pat | R2Dummy deriving (Show,Eq,Ord)
data R2Pat = P2 deriving (Show,Eq,Ord) 

instance CHRRule R2 where
   allPatterns = [R2 P2]

data R3 = R3 R3Pat | R3Dummy deriving (Show,Eq,Ord)
data R3Pat = P3 | P4 deriving (Show,Eq,Ord) 

instance CHRRule R3 where
   allPatterns = [R3 P3,R3 P4]

instance CHROperations R1 Prime () where
   matchPattern (R1 P1) () (Candidate(1)) = Just ()
   matchPattern (R1 P1) _ _ = Nothing 

   checkGuard _ () = True 
   checkGuard _ _ = False 

   instBody _ () = Just [] 
   instBody _ _ = Just [] 


instance CHROperations R2 Prime (Maybe Int) where
   matchPattern (R2 P2) (Just n') (Candidate(n)) | (n==n') = Just (Just n)
   matchPattern (R2 P2) (Nothing) (Candidate(n)) = Just (Just n)
   matchPattern (R2 P2) _ _ = Nothing 

   checkGuard _ (Just n) = True 
   checkGuard _ _ = False 

   instBody _ (Just n) = Just [Prime(n),Candidate(n-1)]
   instBody _ _ = Just [] 


instance CHROperations R3 Prime (Maybe Int,Maybe Int) where
   matchPattern (R3 P3) (Just y',mb2) (Prime(y)) | (y==y') = Just (Just y,mb2)
   matchPattern (R3 P3) (Nothing,mb2) (Prime(y)) = Just (Just y,mb2)
   matchPattern (R3 P3) _ _ = Nothing 

   matchPattern (R3 P4) (mb1,Just x') (Prime(x)) | (x==x') = Just (mb1,Just x)
   matchPattern (R3 P4) (mb1,Nothing) (Prime(x)) = Just (mb1,Just x)
   matchPattern (R3 P4) _ _ = Nothing 

   checkGuard _ (Just y,Just x) = x `mod` y == 0 
   checkGuard _ _ = False 

   instBody _ (Just y,Just x) = Just [Prime(y)]
   instBody _ _ = Just [] 


instance Constraint Prime where 
   derivation _ _ termSig _ [] = doAtomic (writeTVar termSig True)
   derivation (t,i0) sts termSig errSig (ic:ics) = do
            dontDrop <- doAtomic (validateContent ic)
            (if dontDrop then do
               (i1,ics1,b1) <- solverThread (t,i0) R1Dummy sts errSig ic False
               (i2,ics2,b2) <- solverThread (t,i1) R2Dummy sts errSig ic b1
               (i3,ics3,b3) <- solverThread (t,i2) R3Dummy sts errSig ic b2
               let ics' = ics1 ++ ics2 ++ ics3
               derivation (t,i3) sts termSig errSig (ics ++ ics')
             else derivation (t,i0) sts termSig errSig ics)
