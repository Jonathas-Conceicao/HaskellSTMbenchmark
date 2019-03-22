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

import IO 
import System
import System.Time

import CCHR.CCHRCommandLine

data Robot = Ro1 | Ro2 deriving (Eq,Ord,Show)

instance Eq Blockworld where
   Get(r1,s1) == Get(r2,s2) = (r1==r2)&&(s1==s2)
   PutOn(r1,s1) == PutOn(r2,s2) = (r1==r2)&&(s1==s2)
   Holds(r1,s1) == Holds(r2,s2) = (r1==r2)&&(s1==s2)
   EmptyB(r1) == EmptyB(r2) = r1==r2
   On(r1,s1) == On(r2,s2) = (r1==r2)&&(s1==s2)
   Clear(r1) == Clear(r2) = r1==r2
   _ == _ = False

instance Show Blockworld where
   show (Get(r,s)) = "Get(" ++ (show r) ++ "," ++ s ++ ")"
   show (PutOn(r,s)) = "PutOn(" ++ (show r) ++ "," ++ s ++ ")"  
   show (Holds(r,s)) = "Holds(" ++ (show r) ++ "," ++ s ++ ")"
   show (EmptyB(r)) = "EmptyB(" ++ (show r) ++ ")"
   show (On(r,s)) = "On(" ++ r ++ "," ++ s ++ ")"
   show (Clear(s)) = "Clear(" ++ s ++ ")"

simpBW = [ Get(Ro1,"B5a"),PutOn(Ro1,"T1a"),Get(Ro1,"B4a"),PutOn(Ro1,"B5a"),Get(Ro1,"B3a"),PutOn(Ro1,"B4a"),Get(Ro1,"B2a"),
           PutOn(Ro1,"B3a"),Get(Ro1,"B1a"),PutOn(Ro1,"B2a"),EmptyB(Ro1),
           Clear("B5a"),On("B5a","B4a"),On("B4a","B3a"),On("B3a","B2a"),On("B2a","B1a"),On("B1a","T2a"),Clear("T1a"),
           Clear("B5b"),On("B5b","B4b"),On("B4b","B3b"),On("B3b","B2b"),On("B2b","B1b"),On("B1a","T4b"),Clear("T3b")          
         ]

bwInst :: Robot -> String -> Int -> [Blockworld]
bwInst r s i = [Clear("B"++(show i)++s)] ++ (blocks s i) ++ (acts r s i) ++ [Clear("T100"++s)]
               where
                acts r s i  = [Get(r,"B"++(show i)++s),PutOn(r,"T1"++s)] ++ (acts' r s (i-1))
                acts' r s 1 = [Get(r,"B1"++s),PutOn(r,"B2"++s),EmptyB(r)]
                acts' r s i = (acts' r s (i-1)) ++ [Get(r,"B"++(show i)++s),PutOn(r,"B"++(show (i+1))++s)]
                blocks s 1  = [On("B1"++s,"T2"++s),Clear("T1"++s)]
                blocks s i  = (On("B"++(show i)++s,"B"++(show (i-1))++s)):(blocks s (i-1))

bwInst2 :: (Robot,Robot) -> String -> Int -> [Blockworld]
bwInst2 (r1,r2) s i = [Clear("B"++(show i)++s)] ++ (blocks s i) ++ (acts (r1,r2) s i) ++ [Clear("T100"++s)]
                      where
                        acts (r1,r2) s i  = [Get(r1,"B"++(show i)++s),PutOn(r1,"T1"++s)] ++ (acts' (r2,r1) s (i-1))
                        acts' (r1,r2) s 1 = [Get(r1,"B1"++s),PutOn(r1,"B2"++s),EmptyB(r1),EmptyB(r2)]
                        acts' (r1,r2) s i = (acts' (r2,r1) s (i-1)) ++ [Get(r1,"B"++(show i)++s),PutOn(r1,"B"++(show (i+1))++s)]
                        blocks s 1  = [On("B1"++s,"T2"++s),Clear("T1"++s)]
                        blocks s i  = (On("B"++(show i)++s,"B"++(show (i-1))++s)):(blocks s (i-1))

main :: IO ()
main = do
    let bw = (bwInst Ro1 "a" 100) ++ [Clear("T5")] ++ (bwInst Ro2 "b" 100)
    -- let bw = bwInst2 (Ro1,Ro2) "a" 100
    runCommand bw
   -- readTStats

data Blockworld = Get(Robot,String) | PutOn(Robot,String) | Holds(Robot,String) | EmptyB(Robot) | On(String,String) | Clear(String) deriving Ord

data R1 = R1 R1Pat | R1Dummy deriving (Show,Eq,Ord)
data R1Pat = P1 | P2 | P3 | P4 deriving (Show,Eq,Ord) 

instance CHRRule R1 where
   allPatterns = [R1 P1,R1 P2,R1 P3,R1 P4]

data R2 = R2 R2Pat | R2Dummy deriving (Show,Eq,Ord)
data R2Pat = P5 | P6 | P7 deriving (Show,Eq,Ord) 

instance CHRRule R2 where
   allPatterns = [R2 P5,R2 P6,R2 P7]

instance CHROperations R1 Blockworld (Maybe Robot,Maybe String,Maybe String) where
   matchPattern (R1 P1) (Just r',Just m',mb3) (Get(r,m)) | (r==r')&&(m==m') = Just (Just r,Just m,mb3)
   matchPattern (R1 P1) (Just r',Nothing,mb3) (Get(r,m)) | (r==r') = Just (Just r,Just m,mb3)
   matchPattern (R1 P1) (Nothing,Just m',mb3) (Get(r,m)) | (m==m') = Just (Just r,Just m,mb3)
   matchPattern (R1 P1) (Nothing,Nothing,mb3) (Get(r,m)) = Just (Just r,Just m,mb3)
   matchPattern (R1 P1) _ _ = Nothing 

   matchPattern (R1 P2) (Just r',mb2,mb3) (EmptyB(r)) | (r==r') = Just (Just r,mb2,mb3)
   matchPattern (R1 P2) (Nothing,mb2,mb3) (EmptyB(r)) = Just (Just r,mb2,mb3)
   matchPattern (R1 P2) _ _ = Nothing 

   matchPattern (R1 P3) (mb1,Just m',mb3) (Clear(m)) | (m==m') = Just (mb1,Just m,mb3)
   matchPattern (R1 P3) (mb1,Nothing,mb3) (Clear(m)) = Just (mb1,Just m,mb3)
   matchPattern (R1 P3) _ _ = Nothing 

   matchPattern (R1 P4) (mb1,Just m',Just n') (On(m,n)) | (m==m')&&(n==n') = Just (mb1,Just m,Just n)
   matchPattern (R1 P4) (mb1,Just m',Nothing) (On(m,n)) | (m==m') = Just (mb1,Just m,Just n)
   matchPattern (R1 P4) (mb1,Nothing,Just n') (On(m,n)) | (n==n') = Just (mb1,Just m,Just n)
   matchPattern (R1 P4) (mb1,Nothing,Nothing) (On(m,n)) = Just (mb1,Just m,Just n)
   matchPattern (R1 P4) _ _ = Nothing 

   checkGuard _ (Just r,Just m,Just n) = True 
   checkGuard _ _ = False 

   instBody _ (Just r,Just m,Just n) = Just [Holds(r,m),Clear(n)]
   instBody _ _ = Just [] 


instance CHROperations R2 Blockworld (Maybe Robot,Maybe String,Maybe String) where
   matchPattern (R2 P5) (Just r',Just m',mb3) (PutOn(r,m)) | (r==r')&&(m==m') = Just (Just r,Just m,mb3)
   matchPattern (R2 P5) (Just r',Nothing,mb3) (PutOn(r,m)) | (r==r') = Just (Just r,Just m,mb3)
   matchPattern (R2 P5) (Nothing,Just m',mb3) (PutOn(r,m)) | (m==m') = Just (Just r,Just m,mb3)
   matchPattern (R2 P5) (Nothing,Nothing,mb3) (PutOn(r,m)) = Just (Just r,Just m,mb3)
   matchPattern (R2 P5) _ _ = Nothing 

   matchPattern (R2 P6) (Just r',mb2,Just n') (Holds(r,n)) | (r==r')&&(n==n') = Just (Just r,mb2,Just n)
   matchPattern (R2 P6) (Just r',mb2,Nothing) (Holds(r,n)) | (r==r') = Just (Just r,mb2,Just n)
   matchPattern (R2 P6) (Nothing,mb2,Just n') (Holds(r,n)) | (n==n') = Just (Just r,mb2,Just n)
   matchPattern (R2 P6) (Nothing,mb2,Nothing) (Holds(r,n)) = Just (Just r,mb2,Just n)
   matchPattern (R2 P6) _ _ = Nothing 

   matchPattern (R2 P7) (mb1,Just m',mb3) (Clear(m)) | (m==m') = Just (mb1,Just m,mb3)
   matchPattern (R2 P7) (mb1,Nothing,mb3) (Clear(m)) = Just (mb1,Just m,mb3)
   matchPattern (R2 P7) _ _ = Nothing 

   checkGuard _ (Just r,Just m,Just n) = True 
   checkGuard _ _ = False 

   instBody _ (Just r,Just m,Just n) = Just [EmptyB(r),Clear(n),On(n,m)]
   instBody _ _ = Just [] 


instance Constraint Blockworld where 
   derivation _ _ termSig _ [] = doAtomic (writeTVar termSig True)
   derivation (t,i0) sts termSig errSig (ic:ics) = do
            dontDrop <- doAtomic (validateContent ic)
            (if dontDrop then do
               (i1,ics1,b1) <- solverThread (t,i0) R1Dummy sts errSig ic False
               (i2,ics2,b2) <- solverThread (t,i1) R2Dummy sts errSig ic b1
               let ics' = ics1 ++ ics2
               derivation (t,i2) sts termSig errSig (ics ++ ics')
             else derivation (t,i0) sts termSig errSig ics)
