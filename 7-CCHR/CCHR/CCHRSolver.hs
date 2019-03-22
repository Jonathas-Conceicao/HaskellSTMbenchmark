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

module CCHR.CCHRSolver (
   module CCHR.CCHRCore, module CCHR.CCHRStore,
   module CCHR.CCHRSolver
) where

import GHC.Conc
import CCHR.Control.State

import CCHR.CCHRCore
import CCHR.CCHRStore

--------------------------------------------------------------------------------------------
-- CHR Triggering Components 
--------------------------------------------------------------------------------------------

validateAndSimplify :: (CHRRule r,Constraint c,Subst s,CHROperations r c s, SearchInterface m) 
                          => (String,Int) -> r -> Match c s -> TVar Bool -> ConsLoc c -> m (Int,[ConsLoc c],Bool)
validateAndSimplify (s,i) r m@(Match ics sub) stat ic = do
        bs <- doAtomic (mapM validateContent ics)
        case (and bs) of
           False -> return (i,[ic],False)
           True  -> do (j,ics) <- simplify (s,i) r m stat
                       return (j,ics,True)

simplify :: (CHRRule r,Constraint c,Subst s,CHROperations r c s, SearchInterface m) 
               => (String,Int) -> r -> Match c s -> TVar Bool -> m (Int,[ConsLoc c])
simplify (s,i) r (Match ics sub) stat = do
       let mb = instBody r sub
       case mb of
         Just cs' -> do let cls = map intCell ics
                        doAtomic (replaceManyWithLoc (s,i) cls cs')
         Nothing  -> do doAtomic (writeTVar stat True)
                        return (i,[])

tryRule :: (CHRRule r,Constraint c,Subst s,CHROperations r c s,SearchInterface m) 
              => (String,Int) -> r -> ConsLoc c -> [StoreEntry c] -> TVar Bool -> m (Int,[ConsLoc c],Bool)
tryRule (s,i) r ic st tb = do
     mb <- incActiveSearch r ic st
     case mb of
        Nothing -> return (i,[],False)
        Just m  -> validateAndSimplify (s,i) r m tb ic

-- Invoke rule search with atomic hops
solverThread :: (CHRRule r,Constraint c,Subst s,CHROperations r c s,SearchInterface m) => 
                (String,Int) -> r -> [StoreEntry c] -> TVar Bool -> ConsLoc c -> Bool -> m (Int,[ConsLoc c],Bool)
solverThread (_,i) _ _ _ _ True = return (i,[],True)
solverThread (s,i) r st errSig ic False = tryRule (s,i) r ic st errSig

dSolve :: Constraint cons => [cons] -> Int -> (Bool,Bool) -> Solver cons [cons]
dSolve [] _ _ = return []
dSolve cs i (small,dist) = do
      (icss,tbs) <- initCCHR cs i
      sts <- getField store
      ent <- doAtomic (retrieveEntry sts)
      ids <- dSolve' icss tbs ent 1 (small,dist)
      doAtomic (observeActive tbs)
      lift (mapM_ killThread ids)
      getStore
      where
         dSolve' :: Constraint c => [[ConsLoc c]] -> [TVar Bool] -> [StoreEntry c] -> Int -> (Bool,Bool) -> Solver c [ThreadId]
         dSolve' [ics] [tb] st i (small,dist) = do
             Just errSig <- getField unsat
             let cid = "T" ++ (show i)
             lift (derivation' (cid,0) st tb errSig ics small)
             return []
         dSolve' (ics:icss) (tb:tbs) st i (small,dist) = do
             Just errSig <- getField unsat
             let cid = "T" ++ (show i)
             id  <- lift (forkIO (derivation' (cid,0) st tb errSig ics small))
             let st' = if dist then (rotate st) else st
             ids <- dSolve' icss tbs st' (i+1) (small,dist)
             return (id:ids)
         dSolve' [] [] _ _ _ = return []
         derivation' :: Constraint c => (String,Int) -> [StoreEntry c] -> TVar Bool -> TVar Bool -> [ConsLoc c] -> Bool -> IO ()
         derivation' (cid,i) st tb errSig ics True  = derivation (cid,i) st tb errSig ics
         derivation' (cid,i) st tb errSig ics False = atomically (derivation (cid,i) st tb errSig ics)

----------------------------------------------------------------------------
-- Solver State Monad
----------------------------------------------------------------------------

data SolverState cons = SolverState { store  :: [Store cons],
                                      unsat  :: Maybe (TVar Bool),
                                      emptys :: [TVar Bool]
                                    }

type Solver cons a = StateT (SolverState cons) IO a

store' :: ([Store cons] -> [Store cons]) -> SolverState cons -> SolverState cons
store' f st = st { store = f (store st) }

unsat' :: (Maybe (TVar Bool) -> Maybe (TVar Bool)) -> SolverState cons -> SolverState cons
unsat' f st = st { unsat = f (unsat st) }

emptys' :: ([TVar Bool] -> [TVar Bool]) -> SolverState cons -> SolverState cons
emptys' f st = st { emptys = f (emptys st) }

------------------------------------------------------------------------------
-- Auxiliary Function
------------------------------------------------------------------------------

-- Split a list equally into n lists.
splitNWay :: Int -> [a] -> [[a]]
splitNWay n xs = let i = (length xs) `div` n
                     j = ((length xs) `mod` n)
                     s = case (j > n `div` 2) of
                           True  -> i + 1
                           False -> i
                     (xss,[]) = splitNWay' s xs n
                 in xss
                 where
                    splitNWay' :: Int -> [a] -> Int -> ([[a]],[a])
                    splitNWay' i [] 0 = ([],[])
                    splitNWay' i [] n = let (xs,[]) = splitNWay' i [] (n-1)
                                        in ([]:xs,[])
                    splitNWay' i xs 1 = ([xs],[])
                    splitNWay' i xs n = let (xs',xs'')  = splitAt i xs
                                            (xss,xs''') = splitNWay' i xs'' (n-1)
                                        in ((xs':xss),xs''')

-- Rotate a list
rotate :: [a] -> [a]
rotate [] = []
rotate (a:as) = as ++ [a]

removeLabel :: (cons,String) -> cons
removeLabel (c,_) = c

-- loop function to endlessly loop an input IO operation.
loop :: (Int -> IO (Int,a)) -> Int -> IO ()
loop m i = do (j,a) <- m i
              _ <- loop m j
              return ()

observeActive :: [TVar Bool] -> STM ()
observeActive tbs = do
     bs  <- readAllBool tbs
     case bs of
        True  -> return ()
        False -> observeActive tbs
     where
        readAllBool :: [TVar Bool] -> STM Bool
        readAllBool (tb:tbs) = do
            b <- readTVar tb
            case b of
              False -> return False
              True  -> readAllBool tbs
        readAllBool [] = return True

retrieveEntry :: [Store c] -> STM [StoreEntry c]
retrieveEntry [] = return []
retrieveEntry (st:sts) = do
     cl  <- readTVar st
     cls <- retrieveEntry sts
     return (cl:cls)

------------------------------------------------------------------------------
-- Solver monad operations
------------------------------------------------------------------------------

initCCHR :: (Constraint cons) => [cons] -> Int -> Solver cons ([[ConsLoc cons]],[TVar Bool])
initCCHR cs i = do
     initSat
     icss <- initStoreLoc cs i
     tbs <- initEmptys i
     return (icss,tbs)

initializeLoc :: [cons] -> STM (Store cons,[ConsLoc cons])
initializeLoc cs = do
     cl <- newTVar Empty
     st <- newTVar cl
     (j,ics) <- insertManyAtLoc ("r0",0) cs cl
     return (st,ics)

initStoreLoc :: [cons] -> Int -> Solver cons [[ConsLoc cons]]
initStoreLoc cs i = do
     let css = splitNWay i cs
     (sts,icss) <- allocMulti css
     update (store' (replace sts))
     return icss
     where
        allocMulti :: [[cons]] -> Solver cons ([Store cons],[[ConsLoc cons]])
        allocMulti (cs:css) = do
             (sts,icss) <- allocMulti css
             (st,ics) <- doAtomic (initializeLoc cs)
             return (st:sts,ics:icss)
        allocMulti [] = return ([],[])

initSat :: Solver cons ()
initSat = do
    tb <- doAtomic newTBool
    update (unsat' (replace (Just tb)))
    where
       newTBool :: STM (TVar Bool)
       newTBool = newTVar False

initEmptys :: Int -> Solver cons [TVar Bool]
initEmptys i = do
     tbs <- doAtomic (initEmptys' i)
     update (emptys' (replace tbs))
     return tbs
     where
        initEmptys' :: Int -> STM [TVar Bool]
        initEmptys' 0 = return []
        initEmptys' i = do bs <- initEmptys' (i-1)
                           b  <- newTVar False
                           return (b:bs)

runSolver :: Solver cons a -> IO a
runSolver sol = evalStateT sol (SolverState [] Nothing [])

getStore :: Solver cons [cons]
getStore = do
   css <- getField store
   getStore' css
   where
      getStore' :: [Store cons] -> Solver cons [cons]
      getStore' (st:sts) = do
              lcs <- doAtomic (getAll st)
              let cs = map removeLabel lcs
              cs' <- getStore' sts
              return (cs ++ cs')
      getStore' [] = return []

printStore :: Constraint cons => Solver cons ()
printStore = do
   cs <- getStore
   lift (putStrLn (show cs))
