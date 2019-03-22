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

import Char
import IO 
import System
import System.Time

import CCHR.CCHRCommandLine

instance Eq Gcd where

   Gcd(i) == Gcd(j) = (i==j)


instance Show Gcd where
   show (Gcd(i)) = "Gcd(" ++ (show i) ++ ")"

myState = [Gcd(45),Gcd(60),Gcd(150),Gcd(27),Gcd(81),Gcd(810),Gcd(1500)]

moreGcd f i n | n<=i = []
moreGcd f i n | i `mod` 2 == 0 = Gcd(f*i):(moreGcd f (i+1) n) 
moreGcd f i n = (moreGcd f (i+1) n) ++ [Gcd(f*i)]

moreGcd2 i n | n<=i = []
moreGcd2 i n = (moreGcd2 (i+1) n) ++ [Gcd(7*i)]

moreGcd3 i n | n<=i = []
moreGcd3 i n = (Gcd(7*i)):(moreGcd3 (i+1) n)

main :: IO ()
main = do
   -- let gcd = moreGcd 11 2 3000
   let gcd = moreGcd 11 2 3000
   runCommand gcd
   -- readTStats

data Gcd = Gcd(Int) deriving Ord

data R1 = R1 R1Pat | R1Dummy deriving (Show,Eq,Ord)
data R1Pat = P1 deriving (Show,Eq,Ord) 

instance CHRRule R1 where
   allPatterns = [R1 P1]

data R2 = R2 R2Pat | R2Dummy deriving (Show,Eq,Ord)
data R2Pat = P2 | P3 deriving (Show,Eq,Ord) 

instance CHRRule R2 where
   allPatterns = [R2 P2,R2 P3]

instance CHROperations R1 Gcd () where
   matchPattern (R1 P1) () (Gcd(0)) = Just ()
   matchPattern (R1 P1) _ _ = Nothing 

   checkGuard _ () = True 
   checkGuard _ _ = False 

   instBody _ () = Just [] 
   instBody _ _ = Just [] 


instance CHROperations R2 Gcd (Maybe Int,Maybe Int) where
   matchPattern (R2 P2) (Just m',mb2) (Gcd(m)) | (m==m') = Just (Just m,mb2)
   matchPattern (R2 P2) (Nothing,mb2) (Gcd(m)) = Just (Just m,mb2)
   matchPattern (R2 P2) _ _ = Nothing 

   matchPattern (R2 P3) (mb1,Just n') (Gcd(n)) | (n==n') = Just (mb1,Just n)
   matchPattern (R2 P3) (mb1,Nothing) (Gcd(n)) = Just (mb1,Just n)
   matchPattern (R2 P3) _ _ = Nothing 

   checkGuard _ (Just m,Just n) = ((m>=n)&&(m/=0)&&(n/=0)) 
   checkGuard _ _ = False 

   instBody _ (Just m,Just n) = Just [Gcd(m-n),Gcd(n)]
   instBody _ _ = Just [] 


instance Constraint Gcd where 
   derivation _ _ termSig _ [] = doAtomic (writeTVar termSig True)
   derivation (t,i0) sts termSig errSig (ic:ics) = do
            dontDrop <- doAtomic (validateContent ic)
            (if dontDrop then do
               (i1,ics1,b1) <- solverThread (t,i0) R1Dummy sts errSig ic False
               (i2,ics2,b2) <- solverThread (t,i1) R2Dummy sts errSig ic b1
               let ics' = ics1 ++ ics2
               derivation (t,i2) sts termSig errSig (ics ++ ics')
             else derivation (t,i0) sts termSig errSig ics)
