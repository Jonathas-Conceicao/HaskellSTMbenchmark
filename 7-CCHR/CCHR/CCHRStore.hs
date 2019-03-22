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

module CCHR.CCHRStore (
   module CCHR.TList, module CCHR.CCHRStore
) where

import CCHR.Control.State

import CCHR.TList
import GHC.Conc

--------------------------------------------------------
-- Data Types
--------------------------------------------------------

-- Constraint of interest: Contains both the constraint and pointer
-- to the location of the translational list where it should be
-- found. 
data ConsLoc cons = ConsLoc { 
                              intConst :: cons, 
                              intLabel :: String,
                              intCell  :: Cell (cons,String)
                            }

instance Show cons => Show (ConsLoc cons) where
    show (ConsLoc cons l cl) = (show cons) ++ "#" ++ (show l)

instance Eq cons => Eq (ConsLoc cons) where
    (ConsLoc c l _) == (ConsLoc c' l' _) = (c==c') && (l==l')
 
instance Ord cons => Ord (ConsLoc cons) where
    (ConsLoc c l _) > (ConsLoc c' l' _)  = case (c == c') of
                                               True  -> l > l'
                                               False -> c > c' 
    (ConsLoc c l _) < (ConsLoc c' l' _)  = case (c == c') of
                                               True  -> l < l'
                                               False -> c < c'
    (ConsLoc c l _) >= (ConsLoc c' l' _)  = case (c == c') of
                                                True  -> l >= l'
                                                False -> c >= c' 
    (ConsLoc c l _) <= (ConsLoc c' l' _)  = case (c == c') of
                                                True  -> l <= l'
                                                False -> c <= c'

-----------------------------------------------------------------
-- Extended TList Operations for CCHR operations
-----------------------------------------------------------------

-- Given a ConsLoc c l cl containing an item c and cell cl, returns
-- true if c is equal to the contents of cl. If cl is empty,
-- return false, if cl is forwarding node to cl', redirect to cl',
-- otherwise, return false.
validateContent :: Eq cons => ConsLoc cons -> STM Bool
validateContent ic@(ConsLoc c l cl) = do
     sl <- readTVar cl
     case sl of
       Empty          -> return False
       Full (c',l') _ -> return ((c==c')&&(l==l'))
       Forward cl'    -> validateContent (ConsLoc c l cl')

insertManyAtLoc :: (String,Int) -> [cons] -> Cell (cons,String) -> STM (Int,[ConsLoc cons])
insertManyAtLoc (s,i) cs cl = do
     sl <- readTVar cl
     case sl of
        Empty       -> do (j,ics,mb,sl') <- insertManyAtLoc' (s,i) cs Empty
                          writeTVar cl sl'
                          case mb of
                            Just (c,l) -> return (j,(ConsLoc c l cl):ics)
                            Nothing    -> return (j,ics)
        Full c cl'  -> do (j,ics,mb,sl') <- insertManyAtLoc' (s,i) cs sl
                          writeTVar cl sl'
                          case mb of
                            Just (c,l) -> return (j,(ConsLoc c l cl):ics)
                            Nothing    -> return (j,ics)
        Forward cl' -> insertManyAtLoc (s,i) cs cl'
     where
        insertManyAtLoc' :: (String,Int) -> [cons] -> Slot (cons,String) -> 
                             STM (Int,[ConsLoc cons],Maybe (cons,String),Slot (cons,String))
        insertManyAtLoc' (s,i) (c:cs) sl = do
             (j,ics,mb,sl') <- insertManyAtLoc' (s,i) cs sl
             cl <- newTVar sl'
             case mb of
               Just (c',l') -> do let l = s ++ "-" ++ (show j)
                                  return (j+1,(ConsLoc c' l' cl):ics,Just (c,l),Full (c,l) cl)
               Nothing -> do let l = s ++ "-" ++ (show j)
                             return (j+1,ics,Just (c,l),Full (c,l) cl)
        insertManyAtLoc' (s,i) [] sl = do case sl of
                                            Empty        -> return (i,[],Nothing,sl)
                                            Full (c,l) _ -> return (i,[],Just (c,l),sl)
                                            Forward cl'  -> return (i,[],Nothing,sl)

replaceManyWithLoc :: (String,Int) -> [Cell (cons,String)] -> [cons] -> STM (Int,[(ConsLoc cons)])
replaceManyWithLoc (s,i) [cl] [c] = do let l = s ++ "-" ++ (show i)
                                       replaceWith cl (c,l)
                                       return (i+1,[ConsLoc c l cl])
replaceManyWithLoc (s,i) [cl] (c:cs) = do
     let l = s ++ "-" ++ (show i)
     replaceWith cl (c,l)
     (j,ics) <- insertManyAtLoc (s,i+1) cs cl
     return (j,ics)
replaceManyWithLoc (s,i) (cl:cls) [c] = do
     let l = s ++ "-" ++ (show i)
     replaceWith cl (c,l)
     mapM_ deleteAt cls
     return (i+1,[ConsLoc c l cl])
replaceManyWithLoc (_,i) [] []  = return (i,[])
replaceManyWithLoc (_,i) cls [] = do mapM_ deleteAt cls
                                     return (i,[])
replaceManyWithLoc (s,i) (cl:cls) (c:cs) = do
     let l = s ++ "-" ++ (show i)
     replaceWith cl (c,l)
     (j,ics) <- replaceManyWithLoc (s,i+1) cls cs
     return (j,(ConsLoc c l cl):ics)

--------------------------------------------------------------
-- Constraint Store Abstraction
--------------------------------------------------------------

type Store cons = TList (cons,String)
type StoreEntry cons = Cell (cons,String)


class Monad m => SolverInterface m where
    doAtomic :: STM a -> m a

instance SolverInterface STM where
    doAtomic stm = stm

instance SolverInterface IO where
    doAtomic stm = atomically stm

instance SolverInterface m => SolverInterface (StateT st m) where
    doAtomic stm = lift (doAtomic stm)

iterateEntry :: SolverInterface m => [StoreEntry c] -> m (Maybe (ConsLoc c,[StoreEntry c]))
iterateEntry [] = return Nothing
iterateEntry (cl:cls) = do
    sl <- doAtomic (readTVar cl)
    case sl of
       Empty          -> iterateEntry cls
       Full (c,l) cl' -> do let ic = ConsLoc c l cl
                            return (Just (ic,cl':cls))
       Forward cl'    -> iterateEntry (cl':cls)

