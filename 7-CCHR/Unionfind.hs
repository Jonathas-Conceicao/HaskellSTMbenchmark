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

import System
import IO

import CCHR.CCHRCommandLine

instance Eq Unionfind where
   Make(s) == Make(s') = s == s'
   Root(s) == Root(s') = s == s'
   Union(s1,s2) == Union(s1',s2') = (s1==s1') && (s2==s2')
   Link(s1,s2) == Link(s1',s2') = (s1==s1') && (s2==s2')
   Points(s1,s2) == Points(s1',s2') = (s1==s1') && (s2==s2')
   _ == _ = False

instance Show Unionfind where
   show (Make(s)) = "Make(" ++ s ++ ")"
   show (Root(s)) = "Root(" ++ s ++ ")"
   show (Union(s1,s2)) = "Union(" ++ s1 ++ "," ++ s2 ++ ")"
   show (Link(s1,s2)) = "Link(" ++ s1 ++ "," ++ s2 ++ ")"
   show (Points(s1,s2)) = "Points(" ++ s1 ++ "," ++ s2 ++ ")"

makeTree :: String -> Int -> [Unionfind]
makeTree s i = Make(s):(makeTree' s i)
               where
                  makeTree' _ 0 = []
                  makeTree' s i = [Points(s++"L",s),Points(s++"R",s)] ++  
                                  (makeTree' (s++"L") (i-1)) ++ (makeTree' (s++"R") (i-1))

testTree = (makeTree "a" 7) ++ (makeTree "b" 7) ++ [Union("aLLRLLRR","bLLLLRL")]

main :: IO ()
main = do
    let uf = testTree
    runCommand uf
    -- readTStats

data Unionfind = Make(String) | Root(String) | Union(String,String) | Link(String,String) | Points(String,String) deriving Ord

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

data R4 = R4 R4Pat | R4Dummy deriving (Show,Eq,Ord)
data R4Pat = P5 | P6 deriving (Show,Eq,Ord) 

instance CHRRule R4 where
   allPatterns = [R4 P5,R4 P6]

data R5 = R5 R5Pat | R5Dummy deriving (Show,Eq,Ord)
data R5Pat = P7 deriving (Show,Eq,Ord) 

instance CHRRule R5 where
   allPatterns = [R5 P7]

data R6 = R6 R6Pat | R6Dummy deriving (Show,Eq,Ord)
data R6Pat = P8 | P9 | P10 deriving (Show,Eq,Ord) 

instance CHRRule R6 where
   allPatterns = [R6 P8,R6 P9,R6 P10]

instance CHROperations R1 Unionfind (Maybe String) where
   matchPattern (R1 P1) (Just a') (Make(a)) | (a==a') = Just (Just a)
   matchPattern (R1 P1) (Nothing) (Make(a)) = Just (Just a)
   matchPattern (R1 P1) _ _ = Nothing 

   checkGuard _ (Just a) = True 
   checkGuard _ _ = False 

   instBody _ (Just a) = Just [Root(a)]
   instBody _ _ = Just [] 


instance CHROperations R2 Unionfind (Maybe String,Maybe String) where
   matchPattern (R2 P2) (Just a',Just b') (Union(a,b)) | (a==a')&&(b==b') = Just (Just a,Just b)
   matchPattern (R2 P2) (Just a',Nothing) (Union(a,b)) | (a==a') = Just (Just a,Just b)
   matchPattern (R2 P2) (Nothing,Just b') (Union(a,b)) | (b==b') = Just (Just a,Just b)
   matchPattern (R2 P2) (Nothing,Nothing) (Union(a,b)) = Just (Just a,Just b)
   matchPattern (R2 P2) _ _ = Nothing 

   checkGuard _ (Just a,Just b) = True 
   checkGuard _ _ = False 

   instBody _ (Just a,Just b) = Just [Link(a,b)]
   instBody _ _ = Just [] 


instance CHROperations R3 Unionfind (Maybe String,Maybe String,Maybe String) where
   matchPattern (R3 P3) (Just a',Just b',mb3) (Points(a,b)) | (a==a')&&(b==b') = Just (Just a,Just b,mb3)
   matchPattern (R3 P3) (Just a',Nothing,mb3) (Points(a,b)) | (a==a') = Just (Just a,Just b,mb3)
   matchPattern (R3 P3) (Nothing,Just b',mb3) (Points(a,b)) | (b==b') = Just (Just a,Just b,mb3)
   matchPattern (R3 P3) (Nothing,Nothing,mb3) (Points(a,b)) = Just (Just a,Just b,mb3)
   matchPattern (R3 P3) _ _ = Nothing 

   matchPattern (R3 P4) (Just a',mb2,Just c') (Link(a,c)) | (a==a')&&(c==c') = Just (Just a,mb2,Just c)
   matchPattern (R3 P4) (Just a',mb2,Nothing) (Link(a,c)) | (a==a') = Just (Just a,mb2,Just c)
   matchPattern (R3 P4) (Nothing,mb2,Just c') (Link(a,c)) | (c==c') = Just (Just a,mb2,Just c)
   matchPattern (R3 P4) (Nothing,mb2,Nothing) (Link(a,c)) = Just (Just a,mb2,Just c)
   matchPattern (R3 P4) _ _ = Nothing 

   checkGuard _ (Just a,Just b,Just c) = True 
   checkGuard _ _ = False 

   instBody _ (Just a,Just b,Just c) = Just [Points(a,b),Link(b,c)]
   instBody _ _ = Just [] 


instance CHROperations R4 Unionfind (Maybe String,Maybe String,Maybe String) where
   matchPattern (R4 P5) (Just a',Just b',mb3) (Points(a,b)) | (a==a')&&(b==b') = Just (Just a,Just b,mb3)
   matchPattern (R4 P5) (Just a',Nothing,mb3) (Points(a,b)) | (a==a') = Just (Just a,Just b,mb3)
   matchPattern (R4 P5) (Nothing,Just b',mb3) (Points(a,b)) | (b==b') = Just (Just a,Just b,mb3)
   matchPattern (R4 P5) (Nothing,Nothing,mb3) (Points(a,b)) = Just (Just a,Just b,mb3)
   matchPattern (R4 P5) _ _ = Nothing 

   matchPattern (R4 P6) (Just a',mb2,Just c') (Link(c,a)) | (a==a')&&(c==c') = Just (Just a,mb2,Just c)
   matchPattern (R4 P6) (Just a',mb2,Nothing) (Link(c,a)) | (a==a') = Just (Just a,mb2,Just c)
   matchPattern (R4 P6) (Nothing,mb2,Just c') (Link(c,a)) | (c==c') = Just (Just a,mb2,Just c)
   matchPattern (R4 P6) (Nothing,mb2,Nothing) (Link(c,a)) = Just (Just a,mb2,Just c)
   matchPattern (R4 P6) _ _ = Nothing 

   checkGuard _ (Just a,Just b,Just c) = True 
   checkGuard _ _ = False 

   instBody _ (Just a,Just b,Just c) = Just [Points(a,b),Link(c,b)]
   instBody _ _ = Just [] 


instance CHROperations R5 Unionfind (Maybe String) where
   matchPattern (R5 P7) (Just a') (Link(a,x'')) | (a==a')&&(a==x'') = Just (Just a)
   matchPattern (R5 P7) (Nothing) (Link(a,x'')) | (a==x'') = Just (Just a)
   matchPattern (R5 P7) _ _ = Nothing 

   checkGuard _ (Just a) = True 
   checkGuard _ _ = False 

   instBody _ (Just a) = Just [] 
   instBody _ _ = Just [] 


instance CHROperations R6 Unionfind (Maybe String,Maybe String) where
   matchPattern (R6 P8) (Just a',Just b') (Link(a,b)) | (a==a')&&(b==b') = Just (Just a,Just b)
   matchPattern (R6 P8) (Just a',Nothing) (Link(a,b)) | (a==a') = Just (Just a,Just b)
   matchPattern (R6 P8) (Nothing,Just b') (Link(a,b)) | (b==b') = Just (Just a,Just b)
   matchPattern (R6 P8) (Nothing,Nothing) (Link(a,b)) = Just (Just a,Just b)
   matchPattern (R6 P8) _ _ = Nothing 

   matchPattern (R6 P9) (Just a',mb2) (Root(a)) | (a==a') = Just (Just a,mb2)
   matchPattern (R6 P9) (Nothing,mb2) (Root(a)) = Just (Just a,mb2)
   matchPattern (R6 P9) _ _ = Nothing 

   matchPattern (R6 P10) (mb1,Just b') (Root(b)) | (b==b') = Just (mb1,Just b)
   matchPattern (R6 P10) (mb1,Nothing) (Root(b)) = Just (mb1,Just b)
   matchPattern (R6 P10) _ _ = Nothing 

   checkGuard _ (Just a,Just b) = True 
   checkGuard _ _ = False 

   instBody _ (Just a,Just b) = Just [Points(b,a),Root(a)]
   instBody _ _ = Just [] 


instance Constraint Unionfind where 
   derivation _ _ termSig _ [] = doAtomic (writeTVar termSig True)
   derivation (t,i0) sts termSig errSig (ic:ics) = do
            dontDrop <- doAtomic (validateContent ic)
            (if dontDrop then do
               (i1,ics1,b1) <- solverThread (t,i0) R1Dummy sts errSig ic False
               (i2,ics2,b2) <- solverThread (t,i1) R2Dummy sts errSig ic b1
               (i3,ics3,b3) <- solverThread (t,i2) R3Dummy sts errSig ic b2
               (i4,ics4,b4) <- solverThread (t,i3) R4Dummy sts errSig ic b3
               (i5,ics5,b5) <- solverThread (t,i4) R5Dummy sts errSig ic b4
               (i6,ics6,b6) <- solverThread (t,i5) R6Dummy sts errSig ic b5
               let ics' = ics1 ++ ics2 ++ ics3 ++ ics4 ++ ics5 ++ ics6
               derivation (t,i6) sts termSig errSig (ics ++ ics')
             else derivation (t,i0) sts termSig errSig ics)
