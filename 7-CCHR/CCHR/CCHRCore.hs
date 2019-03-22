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

module CCHR.CCHRCore where

import GHC.Conc
import CCHR.Control.State
import qualified Data.Set as S

import CCHR.CCHRStore  

--------------------------------------------------------------
-- Constraint Handling Rule Class
--------------------------------------------------------------

class (Show cons,Eq cons,Ord cons) => Constraint cons where
   derivation :: (SearchInterface m)
                    => (String,Int) -> [StoreEntry cons] -> TVar Bool -> TVar Bool -> [ConsLoc cons] -> m ()
   derivation _ _ _ _ _  = return ()

class (Show id,Eq id,Ord id) => CHRRule id where
   allPatterns :: [id]
   remove  :: [id] -> id -> [id]

   remove (id:ids) id' = case (id==id') of
                            True  -> ids
                            False -> id:(remove ids id')
   remove [] _ = []

class (CHRRule id,Constraint cons,Subst subst) => CHROperations id cons subst | id -> cons subst where
   matchPattern :: id -> subst -> cons -> Maybe subst
   checkGuard   :: id -> subst -> Bool
   instBody     :: id -> subst -> Maybe [cons]

-----------------------------------------------------------
-- Substitution Class
-----------------------------------------------------------

class Eq sub => Subst sub where
   solved :: sub -> Bool  
   emptySubst :: sub

instance Subst () where
   solved () = True
   emptySubst = ()

instance Eq a => Subst (Maybe a) where
   solved (Just _) = True
   solved Nothing  = False
   emptySubst = (Nothing)

instance (Eq a,Eq b) => Subst (Maybe a,Maybe b) where
   solved (Just _,Just _) = True
   solved _ = False
   emptySubst = (Nothing,Nothing)

instance (Eq a,Eq b,Eq c) => Subst (Maybe a,Maybe b,Maybe c) where
   solved (Just _,Just _,Just _) = True
   solved _ = False
   emptySubst = (Nothing,Nothing,Nothing)

instance (Eq a,Eq b,Eq c,Eq d) => Subst (Maybe a,Maybe b,Maybe c,Maybe d) where
   solved (Just _,Just _,Just _,Just _) = True
   solved _ = False
   emptySubst = (Nothing,Nothing,Nothing,Nothing)

instance (Eq a,Eq b,Eq c,Eq d,Eq e) => Subst (Maybe a,Maybe b,Maybe c,Maybe d,Maybe e) where
   solved (Just _,Just _,Just _,Just _,Just _) = True
   solved _ = False
   emptySubst = (Nothing,Nothing,Nothing,Nothing,Nothing)

instance (Eq a,Eq b,Eq c,Eq d,Eq e,Eq f) => Subst (Maybe a,Maybe b,Maybe c,Maybe d,Maybe e,Maybe f) where
   solved (Just _,Just _,Just _,Just _,Just _,Just _) = True
   solved _ = False
   emptySubst = (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing)

instance (Eq a,Eq b,Eq c,Eq d,Eq e,Eq f,Eq g) => Subst (Maybe a,Maybe b,Maybe c,Maybe d,Maybe e,Maybe f,Maybe g) where
   solved (Just _,Just _,Just _,Just _,Just _,Just _,Just _) = True
   solved _ = False
   emptySubst = (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing)

instance (Eq a,Eq b,Eq c,Eq d,Eq e,Eq f,Eq g,Eq h) => Subst (Maybe a,Maybe b,Maybe c,Maybe d,Maybe e,Maybe f,Maybe g,Maybe h) where
   solved (Just _,Just _,Just _,Just _,Just _,Just _,Just _,Just _) = True
   solved _ = False
   emptySubst = (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing)

instance (Eq a,Eq b,Eq c,Eq d,Eq e,Eq f,Eq g,Eq h,Eq i) =>  Subst (Maybe a,Maybe b,Maybe c,Maybe d,Maybe e,Maybe f,Maybe g,Maybe h,Maybe i) where
   solved (Just _,Just _,Just _,Just _,Just _,Just _,Just _,Just _,Just _) = True
   solved _ = False
   emptySubst = (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing)

instance (Eq a,Eq b,Eq c,Eq d,Eq e,Eq f,Eq g,Eq h,Eq i,Eq j) => Subst (Maybe a,Maybe b,Maybe c,Maybe d,Maybe e,Maybe f,Maybe g,Maybe h,Maybe i,Maybe j) where
   solved (Just _,Just _,Just _,Just _,Just _,Just _,Just _,Just _,Just _,Just _) = True
   solved _ = False
   emptySubst = (Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing)

--------------------------------------------------------------
-- State Monad Transformer Functions
--------------------------------------------------------------

update :: Monad m => (st -> st) -> StateT st m ()
update f = StateT (\st -> return ((),f st))

getField :: Monad m => (st -> a) -> StateT st m a
getField f = StateT (\st -> return (f st,st))

identity :: a -> a
identity a = a

replace :: a -> a -> a
replace x y = x

-------------------------------------------------------------------
-- Join Data Types
-------------------------------------------------------------------

-- Data Representation of entries in join table constructed during
-- CHR rule head matching.
-- JoinEntry ps cs s - ps: Unmatched patterns, cs: Interested constraints,
--                     s: Substitution
data JoinEntry pat cons subst = JoinEntry (S.Set pat) (S.Set (ConsLoc cons)) subst

getSubst :: JoinEntry p c s -> s
getSubst (JoinEntry _ _ s) = s

-- Data Representation of a CHR rule match. Contains information of
-- all constraints of interest, and the substitution involved.
data Match cons subst = Match [ConsLoc cons] subst

instance (Show p,Show c) => Show (JoinEntry p c s) where
    show (JoinEntry ps cs s) = "(" ++ (show ps) ++ "," ++ (show cs) ++ ")"

instance (Eq p,Eq c) => Eq (JoinEntry p c s) where
    (JoinEntry ps cs s) == (JoinEntry ps' cs' s') = (ps == ps') && (cs == cs') -- && (s==s') 

instance (Ord p,Ord c) => Ord (JoinEntry p c s) where
    (JoinEntry ps cs _) > (JoinEntry ps' cs' _) = case (ps == ps') of
                                                    True  -> cs > cs'
                                                    False -> ps > ps'
    (JoinEntry ps cs _) < (JoinEntry ps' cs' _) = case (ps == ps') of
                                                    True  -> cs < cs'
                                                    False -> ps < ps'
    (JoinEntry ps cs _) >= (JoinEntry ps' cs' _) = case (ps == ps') of
                                                     True  -> cs >= cs'
                                                     False -> ps >= ps'
    (JoinEntry ps cs _) <= (JoinEntry ps' cs' _) = case (ps == ps') of
                                                     True  -> cs <= cs'
                                                     False -> ps <= ps'

-----------------------------------------------------------------------
-- Auxiliary join operations
-----------------------------------------------------------------------

sameRef :: ConsLoc c -> ConsLoc c -> Bool
sameRef (ConsLoc _ _ cl) (ConsLoc _ _ cl') = cl == cl'

-- Returns True iff join entry has an empty unmatch pattern list. 
isMatch :: (CHRRule p,Subst s,Constraint c,CHROperations p c s) => p -> [p] -> s -> Bool
isMatch p [] s = checkGuard p s
isMatch _ _ _  = False

-- Returns a Match data if join entry describes a complete CHR match.
-- Otherwise returns Nothing.
getMatch :: (CHRRule p,Subst s,Constraint c,CHROperations p c s) => p -> JoinEntry p c s -> Maybe (Match c s)
getMatch p (JoinEntry sps scs subs) = case (isMatch p (S.toList sps) subs) of
                                      True  -> Just (Match (S.toList scs) subs)
                                      False -> Nothing

-- Returns first occurrence of a match in a join table if one
-- exists.
getFirstMatch :: (CHRRule p,Subst s,Constraint c,CHROperations p c s) => p -> [JoinEntry p c s] -> Maybe (Match c s)
getFirstMatch p (j:js) = let mb = getMatch p j
                         in case mb of
                              Nothing -> getFirstMatch p js
                              Just m  -> mb
getFirstMatch _ [] = Nothing

defaultEntry :: (CHRRule p,Subst s,Constraint c,CHROperations p c s) => JoinEntry p c s
defaultEntry = JoinEntry (S.fromList allPatterns) S.empty emptySubst

--------------------------------------------------------------------------------------------
-- Incremental Join Operations
--------------------------------------------------------------------------------------------

-- Given a join entry j and a constraint of interest c, compute all possible (if any) ways of extending j with c.
-- For example: Suppose for rule A(z),A(x),B(y) <==> ... ,
-- j = JoinEntry [A(z),A(x)] [B(5)] [y->5] and c = A(4),
-- extendMatch j c = [JoinEntry [A(x)] [A(4),B(5)] [y->5,z->4],JoinEntry [A(z)] [A(4),B(5)] [y->5,x->4]]
extendMatch :: (CHRRule p,Subst s,Constraint c,CHROperations p c s) 
                 => p -> JoinEntry p c s -> ConsLoc c -> S.Set (JoinEntry p c s)
extendMatch r j@(JoinEntry ps ics s) ic =
      case (S.member ic ics) of
        False -> let js = extendMatch' (S.toList ps) j ic
                 in S.insert j (dropInconsistent r js)
        True  -> S.singleton j
      where
         extendMatch' :: (CHRRule p,Subst s,Constraint c,CHROperations p c s) 
                             => [p] -> JoinEntry p c s -> ConsLoc c -> S.Set (JoinEntry p c s)
         extendMatch' (p:ps) j@(JoinEntry sps sics s) (ic@(ConsLoc c l cl)) =
               let mb = matchPattern p s c
               in case mb of
                    Nothing -> extendMatch' ps j ic
                    Just s' -> let sps'     = S.delete p sps
                                   sics'    = S.insert ic sics
                                   newEntry = JoinEntry sps' sics' s'
                               in S.insert newEntry (extendMatch' ps j ic)
         extendMatch' [] _ _ = S.empty
         dropInconsistent :: (CHRRule p,Subst s,Constraint c,CHROperations p c s) 
                                 => p -> S.Set (JoinEntry p c s) -> S.Set (JoinEntry p c s)
         dropInconsistent r js = 
               S.filter (check r) js
               where
                  check r (JoinEntry _ _ s) = (not (solved s)) || ((solved s) && (checkGuard r s))

incJoin :: (CHRRule p,Subst s,Constraint c,CHROperations p c s) 
            => p -> S.Set (JoinEntry p c s) -> ConsLoc c -> Either (Match c s) (S.Set (JoinEntry p c s))
incJoin p js ic | S.null js = Right S.empty
incJoin p js ic =
     let j   = S.findMin js
         js' = extendMatch p j ic
         mb  = getFirstMatch p (S.toList js')
     in case mb of
          Just m  -> Left m
          Nothing -> let et = incJoin p (S.deleteMin js) ic
                     in case et of
                          Left m     -> Left m
                          Right js'' -> Right (S.union js' js'')

---------------------------------------------------------------------------------------
-- Incremental search operations
---------------------------------------------------------------------------------------

class SolverInterface m => SearchInterface m where
   incSearch       :: (CHRRule r,Subst s,Constraint c,CHROperations r c s) 
                       => S.Set (JoinEntry r c s) -> r -> [StoreEntry c] -> m (Maybe (Match c s))

   incActiveSearch :: (CHRRule r,Subst s,Constraint c,CHROperations r c s) 
                       => r -> ConsLoc c -> [StoreEntry c] -> m (Maybe (Match c s))

   incCompSearch   :: (CHRRule r,Subst s,Constraint c,CHROperations r c s) 
                       => r -> [StoreEntry c] -> m (Maybe (Match c s))

   incSearch js r st = do
         mb <- iterateEntry st
         case mb of
            Nothing       -> return Nothing
            Just (ic,st') -> do let et = incJoin r js ic 
                                case et of
                                  Left m    -> return (Just m)
                                  Right js' -> incSearch js' r st'

   incActiveSearch r ic st = do 
        let mact = makeActiveMatch r ic
        case (S.null mact) of
           False -> incSearch mact r st
           True  -> return Nothing
        where
           makeActiveMatch :: (CHRRule r,Subst s,Constraint c,CHROperations r c s) 
                                => r -> ConsLoc c -> S.Set (JoinEntry r c s)
           makeActiveMatch r ic = let js = extendMatch r defaultEntry ic
                                  in S.delete defaultEntry js

   incCompSearch p st = incSearch (S.singleton defaultEntry) p st

instance SearchInterface IO
instance SearchInterface STM

smallStepSearch :: (CHRRule r,Subst s,Constraint c,CHROperations r c s) 
                     => r -> ConsLoc c -> [StoreEntry c] -> IO (Maybe (Match c s))
smallStepSearch = incActiveSearch

bigStepSearch :: (CHRRule r,Subst s,Constraint c,CHROperations r c s) 
                   => r -> ConsLoc c -> [StoreEntry c] -> STM (Maybe (Match c s))
bigStepSearch = incActiveSearch             
      

