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

module CCHR.TList where

import Data.List
import GHC.Conc
--import Control.Monad.State

----------------------------------------------------------------------------------
-- Data type Declarations
----------------------------------------------------------------------------------

type TList cons = TVar (Cell cons)
type Cell cons  = TVar (Slot cons)

data Slot cons  = Empty | Full cons (Cell cons) | Forward (Cell cons)

----------------------------------------------------------------------------------
-- TList Access Methods
----------------------------------------------------------------------------------

-- Primitive get operation. Given a location of a linked list, attempt to
-- get contents of the cell, together with the location of the next position.
-- Retries if location is empty.
primGetAt :: Cell cons -> STM (cons,Cell cons)
primGetAt cl = do
    sl <- readTVar cl
    case sl of
      Empty       -> retry
      Full c cl'  -> return (c,cl')
      Forward cl' -> primGetAt cl'

-- Standard non-blocking get operation. Returns the
-- contents of the given cell and address to the next cell.
-- If given cell is empty, returns Nothing.
getAt :: Cell cons -> STM (Maybe (cons,Cell cons))
getAt cl = do
    (getAt' cl) `orElse` (return Nothing)
    where
    getAt' cl = do ans <- primGetAt cl
                   return (Just ans)

-- Blocking version of get operation. Retries
-- until cell is no longer empty.
blockGetAt :: Cell cons -> STM (cons,Cell cons)
blockGetAt cl = do
     mb <- getAt cl
     case mb of
       Nothing  -> retry
       Just ans -> return ans


-- Given pointer to the store, traverse the entire, mutable
-- constraint store and returns a list of all constraints.
getAll :: TList cons -> STM [cons]
getAll st = do
     cl <- readTVar st
     getAll' cl
     where
        getAll' :: Cell cons -> STM [cons]
        getAll' cl = do
              sl <- readTVar cl
              case sl of
                Empty       -> return []
                Full c cl'  -> do cs <- getAll' cl'
                                  return (c:cs)
                Forward cl' -> getAll' cl'

---------------------------------------------------------------------------------
-- TList Insertion Methods
---------------------------------------------------------------------------------

-- Given a cell cl', and another cell cl, write into cl' a forwarding
-- address to cl.
forward :: Cell cons -> Cell cons -> STM ()
forward cl' cl = writeTVar cl' (Forward cl)

-- Remove cs located in cell cl. cl is rewritten with the contents of 
-- the next cell cl'. TVar cl' is in turn rewritten with a forwarding 
-- address to cl.
deleteAt :: Cell cons -> STM ()
deleteAt cl = do
      sl <- readTVar cl
      case sl of
         Empty       -> return ()
         Full cs cl' -> do sl' <- readTVar cl'
                           forward cl' cl
                           writeTVar cl sl'
         Forward cl' -> deleteAt cl'

-- Given a list of items cs and a cell cl, insert the list
-- of constraints cs into the store at location cl. Intermediate
-- cells are created implicitly to house each item of cs.
insertManyAt :: [cons] -> Cell cons -> STM ()
insertManyAt cs cl = do
     sl <- readTVar cl
     case sl of
        Empty       -> do sl' <- insertManyAt' cs Empty
                          writeTVar cl sl'
        Full c cl'  -> do sl'' <- readTVar cl'
                          sl'  <- insertManyAt' cs sl''
                          writeTVar cl' sl'
        Forward cl' -> insertManyAt cs cl'
     where
        insertManyAt' :: [cons] -> Slot cons -> STM (Slot cons)
        insertManyAt' (c:cs) sl = do
             sl' <- insertManyAt' cs sl
             cl  <- newTVar sl'
             return (Full c cl)
        insertManyAt' [] sl = return sl

-- Variant of writesAt that writes only one item to location cl
insertAt :: cons -> Cell cons -> STM ()
insertAt c cl = insertManyAt [c] cl

--------------------------------------------------------------------------------
-- 
--------------------------------------------------------------------------------

-- Given a cell, replace it's content with c, while maintaining
-- linked-list structure. Only introduce new mutable cell if
-- cl is tail of the list (ie. Empty).
replaceWith :: Cell cons -> cons -> STM ()
replaceWith cl c = do
     sl <- readTVar cl
     case sl of
        Full _ cl'  -> writeTVar cl (Full c cl')
        Empty       -> do cl' <- newTVar Empty
                          writeTVar cl (Full c cl')
        Forward cl' -> replaceWith cl' c

-- Replace a sequence of cells (linked-list locations) with a list of
-- items. Effects of replaceManyWith cls cs: Contents of cls removed,
-- while items in cs introduced to linked-list.
-- Handles 3 possible cases:
--       i.) case 1: Length of cls == Length of cs
--           Each cell in cls has its content replaced with a unique
--           c in cs.
--      ii.) case 2: Length of cls < Length of cs
--           
--     iii.) case 3: Length of cls > length of cs
--
-- Important!!: Assumes that each cell in the sequence is unique.   
replaceManyWith :: [Cell cons] -> [cons] -> STM ()
replaceManyWith [cl] [c] = replaceWith cl c
replaceManyWith [cl] (c:cs) = do
     replaceWith cl c
     insertManyAt cs cl
replaceManyWith (cl:cls) [c] = do
     replaceWith cl c
     mapM_ deleteAt cls
replaceManyWith [] [] = return ()
replaceManyWith cls [] = mapM_ deleteAt cls
replaceManyWith (cl:cls) (c:cs) = do
     replaceWith cl c
     replaceManyWith cls cs

{-
-- Given a tuple (c,cl) containing an item c and cell cl, returns
-- true if c is equal to the contents of cl. If cl is empty,
-- return false, if cl is forwarding node to cl', redirect to cl',
-- otherwise, return false.
validateContent :: Eq cons => (cons,Cell cons) -> STM Bool
validateContent (c,cl) = do
     sl <- readTVar cl
     case sl of
       Empty       -> return False
       Full c' _   -> return (c == c')
       Forward cl' -> validateContent (c,cl')
-}
