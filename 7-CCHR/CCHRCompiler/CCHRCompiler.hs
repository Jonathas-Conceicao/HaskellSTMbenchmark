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

import CCHRLexer
import CCHRParser

import List
import CCHR.Control.Error
import CCHR.Control.State

import IO
import System

-------------------------------------------------------
-- Printer Class
-------------------------------------------------------

class Printer a where
   printIt  :: a -> String
   printItD :: a -> String -> String
  
   printItD a s = printIt a

instance Printer a => Printer [a] where
   printIt [a]    = printIt a
   printIt (a:as) = (printIt a) ++ "," ++ (printIt as)
   printIt []     = ""
   printItD [a] d    = printItD a d
   printItD (a:as) d = (printItD a d) ++ d ++ (printItD as d)
   printItD [] _     = ""

instance Printer Type where
   printIt (ListType t)      = "[" ++ (printIt t) ++ "]"
   printIt (TupleType ts)    = "(" ++ (printItD ts ",") ++ ")"
   printIt (ConstrType c ts) = "(" ++ c ++ " " ++ (printItD ts " ") ++ ")"
   printIt (ConstType t)     = t
   printIt (VarType v)       = v
   printIt (FuncType t1 t2)  = (printIt t1) ++ " -> " ++ (printIt t2)
   printIt UnknownType       = "?Type"

instance Printer TermPat where
   printIt (Var x)          = x
   printIt (Const c)        = c
   printIt (ListPat1 p1 p2) = (printIt p1) ++ ":" ++ (printIt p2)
   printIt (ListPat2 ps)    = "[" ++ (printItD ps ",") ++ "]"
   printIt (TuplePat ps)    = "(" ++ (printItD ps ",") ++ ")"
   printIt (ConstrPat c ps) = "(" ++ c ++ " " ++ (printItD ps " ") ++ ")"
   printIt (AtPat x p)      = x ++ "@" ++ (printIt p)
   printIt (BrackPat p)     = "(" ++ (printIt p) ++ ")"
   printIt WildCardPat      = "_"

instance Printer TypeSig where
   printIt (TypeSig c ts) = c ++ "(" ++ (printItD ts ",") ++ ")"

instance Printer Pattern where
   printIt (Pattern c ts) = c ++ "(" ++ (printItD ts ",") ++ ")"

instance Printer Guard where
   printIt (Guard g) = g
   printIt NoGuard   = "True"

instance Printer Body where
   printIt (Body b) = b
   printIt NoBody    = "True"
   printIt FalseBody = "False"

instance Printer CHRRule where
   printIt (SimpRule pats NoGuard body) = (printItD pats ",") ++ " <==> " ++ (printIt body)
   printIt (PropRule pats NoGuard body) = (printItD pats ",") ++ " ==> " ++ (printIt body)
   printIt (SimpRule pats g b) = (printItD pats ",") ++ " <==> " ++ (printIt g) ++ " | " ++ (printIt b)
   printIt (PropRule pats g b) = (printItD pats ",") ++ " ==> " ++ (printIt g) ++ " | " ++ (printIt b)

instance Printer Expr where
   printIt (Name n)    = "name " ++ n
   printIt (CHR c)     = "rule " ++ (printIt c)
   printIt (Code s)    = "{\n" ++ s ++ "\n}"
   printIt (Cons c ts) = "constraint " ++ c ++ " = " ++ (printItD ts " | ")
   printIt (Data t ts) = "data " ++ (printIt t) ++ " = " ++ (printItD ts " | ")

-------------------------------------------------------
-- DataTypes
-------------------------------------------------------

type RuleEnv  = (String,[String]) 	     -- Eg. ("R1",["P1","P2","P3"])
type VarEnv   = (String,[(String,Type)]) -- Eg. ("R1",[("x",ConstType "Int"),("y",ConstType "Bool")])
type PatEnv   = (String,Pattern)  	     -- Eg. ("P1",<A(x:xs)>)
type ConsEnv  = (String,[Type])   	     -- Eg. ("Gcd",[ConstType "Int"])
type DataEnv  = (String,[Type])
type BodyEnv  = (String,Body)            -- Eg. ("R1",FalseBody)
type GuardEnv = (String,Guard)           -- Eg. ("R1",NoGuard)

data CHRState = CHRState { exprs       :: [Expr],
                           ruleEnv     :: [RuleEnv],
                           patEnv      :: [PatEnv],
                           consEnv     :: [ConsEnv],
                           varEnv      :: [VarEnv],
                           ruleInd     :: Int,
                           patInd      :: Int,
                           varInd      :: Int,
                           consType    :: String,
                           outputName  :: String,
                           haskCodes   :: [String],
                           outputCodes :: String,
                           bodyEnv     :: [BodyEnv],
                           guardEnv    :: [GuardEnv],
                           dataEnv     :: [DataEnv]
                         }

defaultState :: [Expr] -> CHRState
defaultState es = CHRState es [] [] [] [] 1 1 0 "" "" [] "" [] [] []

printRuleEnv :: [RuleEnv] -> Int -> String
printRuleEnv (r:rs) i = 
         case (i<4) of
            True  -> (printRuleEnv' r) ++ "," ++ (printRuleEnv rs (i+1))
            False -> (printRuleEnv' r) ++ "\n" ++ (printRuleEnv rs 0)
         where
            printRuleEnv' (s,ss) = "(" ++ s ++ " |-> " ++ (show ss) ++ ")"
printRuleEnv [] _ = ""

printVarEnv :: [VarEnv] -> String
printVarEnv (v:vs) = 
        (printVarEnv' v) ++ "\n" ++ (printVarEnv vs)
        where
           printVarEnv' (r,env) = "(" ++ r ++ " |-> [" ++ (printVarEnv'' env) ++ "])"
           printVarEnv'' [e]    = (show e)
           printVarEnv'' (e:es) = (show e) ++ "," ++ (printVarEnv'' es)          
           printVarEnv'' []     = ""
printVarEnv [] = ""

printPatEnv :: [PatEnv] -> Int -> String
printPatEnv (p:ps) i =
        case (i<4) of
           True  -> (printPatEnv' p) ++ "," ++ (printPatEnv ps (i+1))
           False -> (printPatEnv' p) ++ "\n" ++ (printPatEnv ps 0)
        where
           printPatEnv' (s,p) = "(" ++ s ++ " |-> " ++ (printIt p) ++ ")"
printPatEnv [] _ = ""

printConsEnv :: [ConsEnv] -> String
printConsEnv (c:cs) =
         (printConsEnv' c) ++ "\n" ++ (printConsEnv cs)
         where
            printConsEnv' (c,ts) = "(" ++ c ++ " |-> [" ++ (printItD ts ",") ++ "])"
printConsEnv [] = ""

printHaskCodes :: [String] -> String
printHaskCodes (s:ss) = s ++ "\n" ++ (printHaskCodes ss)
printHaskCodes [] = ""



instance Printer CHRState where
   printIt (CHRState es renv penv cenv venv rind pind vind cons out hs _ benv genv denv) =
         "Program:\n" ++ (printItD es "\n") ++ "\nRule Env:\n" ++ (printRuleEnv renv 0) ++ 
         "\nPat Env:\n" ++ (printPatEnv penv 0) ++ "\nCons Env:\n" ++ (printConsEnv cenv) ++
         "\nVar Env:\n" ++ (printVarEnv venv) ++ "\nRule Index: " ++ (show rind) ++ "\nPat Index: " ++
         (show pind) ++ "\nConstraint Type: " ++ cons ++ "\nOutput File Name: " ++ out ++ "\n" ++
         "Haskell Codes:\n" ++ (printHaskCodes hs) ++ "\n"

-------------------------------------------------------
-- CHRState operations
-------------------------------------------------------

ruleEnv' :: ([RuleEnv] -> [RuleEnv]) -> CHRState -> CHRState
ruleEnv' f st = st { ruleEnv = f (ruleEnv st) }

patEnv' :: ([PatEnv] -> [PatEnv]) -> CHRState -> CHRState
patEnv' f st = st { patEnv = f (patEnv st) }

consEnv' :: ([ConsEnv] -> [ConsEnv]) -> CHRState -> CHRState
consEnv' f st = st { consEnv = f (consEnv st) }

varEnv' :: ([VarEnv] -> [VarEnv]) -> CHRState -> CHRState
varEnv' f st = st { varEnv = f (varEnv st) }

ruleInd' :: (Int -> Int) -> CHRState -> CHRState
ruleInd' f st = st { ruleInd = f (ruleInd st) }

patInd' :: (Int -> Int) -> CHRState -> CHRState
patInd' f st = st { patInd = f (patInd st) }

varInd' :: (Int -> Int) -> CHRState -> CHRState
varInd' f st = st { varInd = f (varInd st) }

consType' :: (String -> String) -> CHRState -> CHRState
consType' f st = st { consType = f (consType st) }

outputName' :: (String -> String) -> CHRState -> CHRState
outputName' f st = st { outputName = f (outputName st) }
 
haskCodes' :: ([String] -> [String]) -> CHRState -> CHRState
haskCodes' f st = st { haskCodes = f (haskCodes st) }

outputCodes' :: (String -> String) -> CHRState -> CHRState
outputCodes' f st = st { outputCodes = f (outputCodes st) }

bodyEnv' :: ([BodyEnv] -> [BodyEnv]) -> CHRState -> CHRState
bodyEnv' f st = st { bodyEnv = f (bodyEnv st) }

guardEnv' :: ([GuardEnv] -> [GuardEnv]) -> CHRState -> CHRState
guardEnv' f st = st { guardEnv = f (guardEnv st) }

dataEnv' :: ([DataEnv] -> [DataEnv]) -> CHRState -> CHRState
dataEnv' f st = st { dataEnv = f (dataEnv st) }

lookupEnv :: String -> [(String,a)] -> Maybe a
lookupEnv s ((s',a):es) = case (s==s') of
                            True  -> Just a
                            False -> lookupEnv s es
lookupEnv _ [] = Nothing

extend :: String -> a -> [(String,a)] -> [(String,a)]
extend s a ((s',a'):es) = case (s==s') of
                             True  -> (s,a):es
                             False -> (s',a'):(extend s a es)
extend s a [] = [(s,a)]

-------------------------------------------------------
-- Translator Monad
-------------------------------------------------------

data TransError = TransError String | ProcRulePat String | NoDec String | MultipleDec String
                | WrongConsCardin String | TypeError String

instance Show TransError where
   show (TransError s)      = "TransError: " ++ s
   show (ProcRulePat s)     = "ProcRulePat: " ++ s
   show (NoDec s)           = "NoDec: " ++ s
   show (MultipleDec s)     = "MultipleDec: " ++ s
   show (WrongConsCardin s) = "WrongConsCardin " ++ s
   show (TypeError s)       = "TypeError " ++ s

instance Error TransError where
   noMsg    = TransError ""
   strMsg s = TransError s

type Translator a = StateT CHRState (ErrorT TransError IO) a

update :: Monad m => (st -> st) -> StateT st m ()
update f = StateT (\st -> return ((),f st))

getField :: Monad m => (st -> a) -> StateT st m a
getField f = StateT (\st -> return (f st,st))

identity :: a -> a
identity a = a

replace :: ((a -> a) -> CHRState -> CHRState) -> a -> Translator ()
replace f a = update (f (replace' a))
              where
                 replace' a b = a

lookupTransEnv :: (CHRState -> [(String,a)]) -> String -> Translator (Maybe a)
lookupTransEnv f s = do env <- getField f
                        return (lookupEnv s env)

lookupVarEnv :: String -> String -> Translator (Maybe Type)
lookupVarEnv r v = do env <- getField varEnv
                      let mb = lookupEnv r env
                      case mb of
                        Nothing   -> return Nothing
                        Just env' -> return (lookupEnv v env')

incRuleInd :: Translator ()
incRuleInd = update (ruleInd' (1+))

incPatInd :: Translator ()
incPatInd = update (patInd' (1+))

incVarInd :: Translator ()
incVarInd = update (varInd' (1+))

resetVarInd :: Translator ()
resetVarInd = replace varInd' 1

newVar :: Translator String
newVar = do
   i <- getField varInd
   case (i>=(length vars)) of
     False -> do incVarInd
                 return (vars !! i)
     True  -> do incVarInd
                 return ("v" ++ (show i))
   where
      vars :: [String]
      vars = ["x''","y''","z''","i''","j''","k''","m''","n''","p''","r''","s''","t''","u''","v''","w''","a''","b''","c''","d''","e''","f''"]

extendRuleEnv :: String -> [String] -> Translator ()
extendRuleEnv s ss = update (ruleEnv' (extend s ss))

extendPatEnv :: String -> Pattern -> Translator ()
extendPatEnv s pat = update (patEnv' (extend s pat))

extendConsEnv :: String -> [Type] -> Translator ()
extendConsEnv s cs = update (consEnv' (extend s cs))

extendBodyEnv :: String -> Body -> Translator ()
extendBodyEnv s b = update (bodyEnv' (extend s b))

extendGuardEnv :: String -> Guard -> Translator ()
extendGuardEnv s g = update (guardEnv' (extend s g))

extendDataEnv :: String -> [Type] -> Translator ()
extendDataEnv s ts = update (dataEnv' (extend s ts))

extendVarEnv :: String -> String -> Type -> Translator ()
extendVarEnv s v t = do mb <- lookupTransEnv varEnv s
                        case mb of
                          Nothing  -> do let env = [(v,t)]
                                         update (varEnv' (extend s env)) 
                          Just env -> do let env' = extend v t env
                                         update (varEnv' (extend s env'))

retrieveDec :: Expr -> [Expr] -> Translator [Expr]
retrieveDec p@(Cons _ _) ((Cons s ts):es) = do cons <- retrieveDec p es
                                               return ((Cons s ts):cons)
retrieveDec p@(Name _) ((Name s):es) = do names <- retrieveDec p es
                                          return ((Name s):names)
retrieveDec p@(Code _) ((Code s):es) = do codes <- retrieveDec p es
                                          return ((Code s):codes)
retrieveDec p@(CHR _) ((CHR c):es) = do chrs <- retrieveDec p es
                                        return ((CHR c):chrs)
retrieveDec p@(Data _ _) ((Data t ts):es) = do dats <- retrieveDec p es
                                               return ((Data t ts):dats) 
retrieveDec p (e:es) = retrieveDec p es
retrieveDec _ [] = return []

retrieveNameDec :: [Expr] -> Translator Expr
retrieveNameDec es = do
        names <- retrieveDec (Name "") es
        case names of
          [name] -> return name
          []     -> lift (throwError (NoDec "No Module Name Declarations"))
          _      -> lift (throwError (MultipleDec "Multiple Module Name Declarations"))

retrieveConstraintDec :: [Expr] -> Translator Expr
retrieveConstraintDec es = do 
        cons <- retrieveDec (Cons "" []) es
        case cons of
           [con] -> return con
           []    -> lift (throwError (NoDec "No Constraint Declarations"))
           _     -> lift (throwError (MultipleDec "Multiple Constraint Declarations"))

retrieveDataDec :: [Expr] -> Translator [Expr]
retrieveDataDec es = retrieveDec (Data UnknownType []) es 

--------------------------------------------------------------------
-- Translation Preprocessing Operations
--------------------------------------------------------------------

preprocessModName :: Translator () 
preprocessModName = do
      es     <- getField exprs
      Name n <- retrieveNameDec es
      replace outputName' n

preprocessDataDecs :: Translator ()
preprocessDataDecs = do
      es   <- getField exprs
      dats <- retrieveDataDec es
      preprocessDataDecs' dats
      where
         preprocessDataDecs' :: [Expr] -> Translator ()
         preprocessDataDecs' ((Data t ts):dats) = do
               preprocessDataDecs'' ts
               preprocessDataDecs' dats
         preprocessDataDecs' [] = return ()
         preprocessDataDecs'' :: [TypeSig] -> Translator ()
         preprocessDataDecs'' ((TypeSig s t):ts) = do
               extendDataEnv s t
               preprocessDataDecs'' ts
         preprocessDataDecs'' [] = return ()
         

preprocessConsTypes :: Translator ()
preprocessConsTypes = do
      es           <- getField exprs
      (Cons s tss) <- retrieveConstraintDec es
      replace consType' s 
      preprocessConsTypes' tss 
      where
         preprocessConsTypes' :: [TypeSig] -> Translator ()
         preprocessConsTypes' ((TypeSig s ts):tss) = do
                extendConsEnv s ts
                preprocessConsTypes' tss
         preprocessConsTypes' [] = return ()

preprocessCHRRules :: Translator ()
preprocessCHRRules = do 
      es   <- getField exprs
      chrs <- retrieveDec dummyRule es
      preprocessCHRRules' chrs
      where
         dummyRule = CHR (SimpRule [] NoGuard NoBody)
         getPatterns (SimpRule pats _ _) = return pats
         getPatterns (PropRule pats _ _) = return pats
         getBodyGuard (SimpRule _ g b) = return (b,g)
         getBodyGuard (PropRule _ g b) = return (b,g)
         preprocessCHRRules' :: [Expr] -> Translator ()
         preprocessCHRRules' ((CHR c):chrs) = do
               i <- getField ruleInd
               let ruleSym = "R" ++ (show i)
               incRuleInd 
               pats    <- getPatterns c
               patSyms <- preprocessPatterns ruleSym pats
               extendRuleEnv ruleSym patSyms    
               (b,g)   <- getBodyGuard c
               extendBodyEnv ruleSym b
               extendGuardEnv ruleSym g           
               preprocessCHRRules' chrs
         preprocessCHRRules' [] = return ()

preprocessPatterns :: String -> [Pattern] -> Translator [String]
preprocessPatterns ruleSym ((pat@(Pattern cons ps)):pats) = do
      i <- getField patInd
      let patSym = "P" ++ (show i)
      incPatInd
      extendPatEnv patSym pat
      mb <- lookupTransEnv consEnv cons
      case mb of
         Nothing -> lift (throwError (TransError "Error in PreprocessPatterns"))
         Just ts -> do preprocessVariables ruleSym ts ps
                       patSyms <- preprocessPatterns ruleSym pats
                       return (patSym:patSyms)
preprocessPatterns _ [] = return []

preprocessVariables :: String -> [Type] -> [TermPat] -> Translator ()
preprocessVariables ruleSym (t:ts) (p:ps) = do
       preprocessVariable ruleSym t p
       preprocessVariables ruleSym ts ps
preprocessVariables _ [] [] = return ()
preprocessVariables _ t p = lift (throwError (WrongConsCardin ((printItD t " | ") ++ " " ++ (printItD p " | "))))

preprocessVariable :: String -> Type -> TermPat -> Translator ()
preprocessVariable _ UnknownType t = lift (throwError (typeErrorReport UnknownType t))
preprocessVariable r t (Var v) = extendVarEnv r v t
preprocessVariable r (ConstType _) (Const _)   = return ()
preprocessVariable r t@(_)         p@(Const _) = lift (throwError (typeErrorReport t p))
preprocessVariable r (t@(ListType t')) (ListPat1 p1 p2) = do
       preprocessVariable r t' p1
       preprocessVariable r t p2
preprocessVariable _ t@(_) p@(ListPat1 _ _) = lift (throwError (typeErrorReport t p))
preprocessVariable r (ListType t) (ListPat2 ps)   = mapM_ (preprocessVariable r t) ps
preprocessVariable _ t@(_)        p@(ListPat2 _)  = lift (throwError (typeErrorReport t p))
preprocessVariable r (TupleType ts) (TuplePat ps)   = preprocessVariables r ts ps
preprocessVariable _ t@(_)          p@(TuplePat _)  = lift (throwError (typeErrorReport t p))
preprocessVariable r t p@(ConstrPat s' ps) = do
       mb <- lookupTransEnv dataEnv s'
       case mb of
          Just ts -> preprocessVariables r ts ps
          Nothing -> lift (throwError (TypeError ("Data Constructor " ++ s' ++ " not found.")))
preprocessVariable r t (AtPat v p) = do
       extendVarEnv r v t
       preprocessVariable r t p
preprocessVariable r t (BrackPat p) = preprocessVariable r t p
preprocessVariable _ _ WildCardPat = return ()

preprocessHaskCodes :: Translator ()
preprocessHaskCodes = do
       es <- getField exprs
       cs <- retrieveDec (Code "") es
       preprocessHaskCodes' cs
       where
          preprocessHaskCodes' :: [Expr] -> Translator ()
          preprocessHaskCodes' ((Code h):cs) = do
                 update (haskCodes' (h:))
                 preprocessHaskCodes' cs
          preprocessHaskCodes' [] = return ()

typeErrorReport :: Type -> TermPat -> TransError
typeErrorReport t p = TypeError ("Attempted to match pattern " ++ (printIt p) ++ " to type " ++ (printIt t) ++ ".")

printState :: Translator ()
printState = do
     st <- get
     lift (lift (putStrLn (printIt st)))

getRules :: Translator [String]
getRules = do
   renv <- getField ruleEnv
   return (getRules' renv)
   where
      getRules' :: [(String,[String])] ->  [String]
      getRules' ((r,_):rs) = r:(getRules' rs)
      getRules' [] = []

--------------------------------------------------------
-- Internal Data Structures & Operations
--------------------------------------------------------

-- Equality Context, eg. EqContext s1 s2 represents s1 == s2
-- This data type is used to handle non-linear patterns by
-- maintaining equality restrictions between renamed variables
-- after patterns have been made linear.
data EqContext = EqContext String String

-- Selection, representing a selected field from a tuple
-- substitution. For example, Select i x, symbolically
-- represent the i-th field of a substitution to be
-- reference by the variable x.
data Select = Select Int String 

instance Eq Select where
   (Select i x) == (Select j y) = (i==j) && (x==y)

instance Ord Select where
   (Select i _) >  (Select j _) = i > j
   (Select i _) >= (Select j _) = i >= j
   (Select i _) <  (Select j _) = i < j
   (Select i _) <= (Select j _) = i <= j

setUnion :: Eq a => [a] -> [a] -> [a]
setUnion as (b:bs) = case (elem b as) of
                       False -> b:(setUnion as bs)
                       True  -> setUnion as bs
setUnion as [] = as

makeSelections :: Pattern -> [(String,Type)] -> [Select]
makeSelections (Pattern a (t:ts)) venv =
    let sl = makeSelection t venv
    in setUnion sl (makeSelections (Pattern a ts) venv)
    where
       makeSelection :: TermPat -> [(String,Type)] -> [Select]
       makeSelection (Var x) venv     = makeSelection' x venv 1
       makeSelection (Const _) venv   = []
       makeSelection (ListPat1 t1 t2) venv = let s1 = makeSelection t1 venv
                                                 s2 = makeSelection t2 venv
                                             in setUnion s1 s2
       makeSelection (ListPat2 ts) venv    = makeSelection'' ts venv
       makeSelection (TuplePat ts) venv    = makeSelection'' ts venv
       makeSelection (ConstrPat _ ts) venv = makeSelection'' ts venv
       makeSelection (AtPat x t) venv      = let s = makeSelection' x venv 1
                                             in setUnion s (makeSelection t venv)
       makeSelection (BrackPat t) venv     = makeSelection t venv
       makeSelection WildCardPat _         = []
       makeSelection' :: String -> [(String,Type)] -> Int -> [Select]
       makeSelection' x ((x',_):venv) i = 
           case (x==x') of
              True  -> [Select i x]
              False -> makeSelection' x venv (i+1)
       makeSelection' _ [] _ = []
       makeSelection'' :: [TermPat] -> [(String,Type)] -> [Select]
       makeSelection'' (t:ts) venv =
           let s1 = makeSelection t venv
               s2 = makeSelection'' ts venv
           in setUnion s1 s2
       makeSelection'' [] _ = []
makeSelections (Pattern _ []) _ = []

data Subst = Subst [Map]
data Map   = Empty | Cont String | Any Int

instance Printer Map where
   printIt Empty    = "Nothing"
   printIt (Cont s) = "Just " ++ s
   printIt (Any i)  = "mb" ++ (show i)

instance Printer Subst where
   printIt (Subst ms) = "(" ++ (printItD ms ",") ++ ")"

instance Printer EqContext where
   printIt (EqContext s1 s2) = "(" ++ s1 ++ "==" ++ s2 ++ ")"

printSubs :: [(Subst,[EqContext])] -> String
printSubs ((subs,eqs):senv) = (printIt subs) ++ " [" ++ (printItD eqs ",") ++ "] \n " ++ (printSubs senv)
printSubs [] = ""

makeEqContexts :: [String] -> String
makeEqContexts [i]    = "(" ++ i ++ "==" ++ i ++ "')"
makeEqContexts (i:is) = "(" ++ i ++ "==" ++ i ++ "')&&" ++ (makeEqContexts is)
makeEqContexts []     = ""

makeSubstType :: [(String,Type)] -> String
makeSubstType senv = 
       "(" ++ (makeSubstType' senv) ++ ")"
       where
          makeSubstType' :: [(String,Type)] -> String
          makeSubstType' [(_,t)]      = "Maybe " ++ (printIt t)
          makeSubstType' ((_,t):senv) = "Maybe " ++ (printIt t) ++ "," ++ (makeSubstType' senv)
          makeSubstType' []           = ""

makeCompleteSubst :: [(String,Type)] -> String
makeCompleteSubst senv =
       "(" ++ (makeCompleteSubst' senv) ++ ")"
       where
          makeCompleteSubst' :: [(String,Type)] -> String
          makeCompleteSubst' [(x,_)]      = "Just " ++ x
          makeCompleteSubst' ((x,_):senv) = "Just " ++ x ++ "," ++ (makeCompleteSubst' senv)
          makeCompleteSubst' []           = ""
 
makeSubstInst :: [Select] -> [(String,Type)] -> Subst
makeSubstInst sels senv = 
       Subst (makeSubstInst' sels 1 (length senv))
       where
          makeSubstInst' :: [Select] -> Int -> Int -> [Map]
          makeSubstInst' as@((Select i x):sels) j max =
              case (i==j) of
                True  -> (Cont x):(makeSubstInst' sels (j+1) max)
                False -> (Any j):(makeSubstInst' as (j+1) max)
          makeSubstInst' [] j max = 
              case (j <= max) of
                True  -> (Any j):(makeSubstInst' [] (j+1) max)
                False -> []

permutateSubst :: Subst -> [(Subst,[EqContext])]
permutateSubst (Subst ms) =
     let max  = noOfCont ms
         subs = permutateSubst' [] [(Subst ms)] 0 max
     in map makeEqs subs
     where
        makeEqs :: Subst -> (Subst,[EqContext]) 
        makeEqs (Subst ((Cont x):ms)) = 
            let (Subst ms',eqs) = makeEqs (Subst ms)
            in (Subst ((Cont (x++"'")):ms'),(EqContext x (x++"'")):eqs)
        makeEqs (Subst (m:ms)) =
            let (Subst ms',eqs) = makeEqs (Subst ms)
            in (Subst (m:ms'),eqs)
        makeEqs (Subst []) = (Subst [],[])
        noOfCont :: [Map] -> Int
        noOfCont ((Cont _):ms) = 1 + (noOfCont ms)
        noOfCont (m:ms) = noOfCont ms
        noOfCont []     = 0
        permutateSubst' :: [Subst] -> [Subst] -> Int -> Int -> [Subst]
        permutateSubst' cu ((Subst ms):subs) i max = 
               let subs' = permutateSubst'' ms i
               in permutateSubst' (cu ++ subs') subs i max
        permutateSubst' cu [] i max = 
               case (i<max-1) of
                  True  -> permutateSubst' [] cu (i+1) max
                  False -> cu
        permutateSubst'' :: [Map] -> Int -> [Subst]
        permutateSubst'' ((Cont x):ms) i =
               case (i<1) of
                  True  -> [Subst ((Cont x):ms), Subst (Empty:ms)]         
                  False -> let subs = permutateSubst'' ms (i-1)
                           in case subs of
                                [Subst ms']            -> [Subst ((Cont x):ms')]
                                [Subst ms',Subst ms''] -> [Subst ((Cont x):ms'),Subst ((Cont x):ms'')]            
        permutateSubst'' (Empty:ms) i =
               let subs = permutateSubst'' ms (i-1)
               in case subs of
                     [Subst ms']            -> [Subst (Empty:ms')]
                     [Subst ms',Subst ms''] -> [Subst (Empty:ms'),Subst (Empty:ms'')]  
        permutateSubst'' (m:ms) i =
               let subs = permutateSubst'' ms i
               in case subs of
                     [Subst ms']            -> [Subst (m:ms')]
                     [Subst ms',Subst ms''] -> [Subst (m:ms'),Subst (m:ms'')]  
        permutateSubst'' [] i = [Subst []]

testSubst = makeSubstInst [Select 1 "z",Select 3 "x",Select 5 "y"] [("",UnknownType),("",UnknownType),("",UnknownType),("",UnknownType),("",UnknownType)]

makeLinearPat :: Pattern -> [String] -> Translator (Pattern,[EqContext])
makeLinearPat (Pattern s ts) env = do
     (ts',eqs,_) <- makeLinearPat' ts env
     return (Pattern s ts',eqs)
     where
        makeLinearPat' :: [TermPat] -> [String] -> Translator ([TermPat],[EqContext],[String])
        makeLinearPat' (t:ts) env = do
            (t',eqs,env')    <- makeLinearPat'' t env
            (ts',eqs',env'') <- makeLinearPat' ts env'
            return (t':ts',eqs ++ eqs',env'')
        makeLinearPat' [] env = return ([],[],env)
        makeLinearPat'' :: TermPat -> [String] -> Translator (TermPat,[EqContext],[String])
        makeLinearPat'' t@(Var x) env = do
            case (elem x env) of
              False -> return (t,[],x:env)
              True  -> do y <- newVar
                          return (Var y,[EqContext x y],y:env)
        makeLinearPat'' t@(ListPat1 t1 t2) env = do
            (t1',eqs,env')   <- makeLinearPat'' t1 env
            (t2',eqs',env'') <- makeLinearPat'' t2 env'
            return (ListPat1 t1' t2',eqs ++ eqs',env'')
        makeLinearPat'' (ListPat2 ts) env = do
            (ts',eqs,env') <- makeLinearPat' ts env
            return (ListPat2 ts',eqs,env')
        makeLinearPat'' (TuplePat ts) env = do
            (ts',eqs,env') <- makeLinearPat' ts env
            return (TuplePat ts',eqs,env')
        makeLinearPat'' (ConstrPat s ts) env = do
            (ts',eqs,env') <- makeLinearPat' ts env
            return (ConstrPat s ts',eqs,env')
        makeLinearPat'' (AtPat s t) env = do
            (t',eqs,env') <- makeLinearPat'' t env
            case (elem s env') of
               False -> return (AtPat s t',eqs,s:env')
               True  -> do y <- newVar
                           return (AtPat y t',(EqContext s y):eqs,y:env')
        makeLinearPat'' (BrackPat t) env = do
            (t',eqs,env') <- makeLinearPat'' t env
            return (BrackPat t',eqs,env')
        makeLinearPat'' t env = return (t,[],env)

-------------------------------------------------------
-- Translator File Output Operations
-------------------------------------------------------

outputModuleName :: Translator ()
outputModuleName = do
      name <- getField outputName
      let imps = ["import CCHR.CCHRSolver","import GHC.Conc","import CCHR.Control.State"]
          mod  = "module " ++ name ++ " where\n\n"
          mod' = (makeSeq 0 imps) ++ "\n"
      lift (lift (writeFile (name ++ ".hs") mod'))

outputConstrDec :: Translator ()
outputConstrDec = do
      name <- getField outputName
      cenv <- getField consEnv
      cons <- getField consType
      let consDec  = "data " ++ cons ++ " = "
          consDec' = consDec ++ (outputCons cenv) ++ " deriving Ord\n\n"
      lift (lift (appendFile (name ++ ".hs") consDec'))
      where
         outputCons :: [(String,[Type])] -> String
         outputCons [(s,ts)] = s ++ "(" ++ (printItD ts ",") ++ ")"
         outputCons ((s,ts):envs) = s ++ "(" ++ (printItD ts ",") ++ ") | " ++ (outputCons envs)
         outputCons [] = ""

outputRulePatDec :: Translator ()
outputRulePatDec = do
      name <- getField outputName
      renv <- getField ruleEnv
      outputRulePatDec' name renv
      where
         outputRulePatDec' :: String -> [(String,[String])] -> Translator ()
         outputRulePatDec' name ((r,ps):renvs) = do
               let ruleDec  = "data " ++ r ++ " = " ++ r ++ " " ++ r ++ "Pat | " ++ r ++ "Dummy" ++ 
                              " deriving (Show,Eq,Ord)" ++ "\n"
                   patDecs  = "data " ++ r ++ "Pat = " ++ (outputPat ps) ++ " deriving (Show,Eq,Ord) \n\n"
                   --ruleEq   = "instance Eq " ++ r ++ " where\n"
                   --ruleEq'  = ruleEq ++ "   (" ++ r ++ " p) == (" ++ r ++ " p') = (p==p')\n\n" 
                   --patEq    = "instance Eq " ++ r ++ "Pat where\n"
                   --patEq'   = patEq ++ (outputPatEq ps) ++ "\n\n"
                   ruleCHR  = "instance CHRRule " ++ r ++ " where\n" ++
                              "   allPatterns = [" ++ (outputAllPats r ps) ++ "]\n\n"
               lift (lift (appendFile (name ++ ".hs") (ruleDec ++ patDecs ++ ruleCHR)))
               outputRulePatDec' name renvs
         outputRulePatDec' _ [] = return ()
         outputPat :: [String] -> String
         outputPat [p] = p
         outputPat (p:ps) = p ++ " | " ++ (outputPat ps)
         outputPat [] = ""             
         outputPatEq :: [String] -> String
         outputPatEq (p:ps) = "   " ++ p ++ " == " ++ p ++ " = True \n" ++ (outputPatEq ps)
         outputPatEq []     = "   _  == _  = False"
         outputAllPats :: String -> [String] -> String
         outputAllPats r [p]    = r ++ " " ++ p
         outputAllPats r (p:ps) = r ++ " " ++ p ++ "," ++ (outputAllPats r ps)
         outputAllPats _ []     = ""

outputHaskCodes :: Translator ()
outputHaskCodes = do
      name <- getField outputName
      hs <- getField haskCodes
      outputHaskCodes' name hs
      where
         outputHaskCodes' :: String -> [String] -> Translator ()
         outputHaskCodes' name (s:ss) = do  
               lift (lift (appendFile (name ++ ".hs") (s ++ "\n")))
               outputHaskCodes' name ss
         outputHaskCodes' _ [] = return ()

outputCHRRules :: Translator ()
outputCHRRules = do
      name <- getField outputName
      rs   <- getRules  
      outputCHRRules' name rs
      where
         outputCHRRules' :: String -> [String] -> Translator ()
         outputCHRRules' name (r:rs) = do
            cons <- getField consType
            senv <- lookupTransEnv' varEnv r 
            let subType = makeSubstType senv
                header  = "instance CHROperations " ++ r ++ " " ++ cons ++ " " ++ 
                          subType ++ " where\n"
            Just ps <- lookupTransEnv ruleEnv r  
            pats    <- makePatterns r ps senv
            guardBody <- makeGuardBody r senv
            lift (lift (appendFile (name ++ ".hs") (header ++ pats ++ guardBody ++ "\n")))
            outputCHRRules' name rs
            where
               lookupTransEnv' venv r = do 
                     mb <- lookupTransEnv venv r
                     case mb of
                       Nothing   -> return []
                       Just senv -> return senv
         outputCHRRules' _ [] = return ()
         makeGuardBody :: String -> [(String,Type)] -> Translator String
         makeGuardBody r senv = do
             Just g <- lookupTransEnv guardEnv r
             Just b <- lookupTransEnv bodyEnv r
             let gDec = "   checkGuard _ " ++ (makeCompleteSubst senv) ++ " = " ++ (makeGuard g) ++ "\n"
                 bDec = "   instBody _ " ++ (makeCompleteSubst senv) ++ " = " ++ (makeBody b) ++ "\n"
             return (gDec ++ bDec)
             where
                makeGuard :: Guard -> String
                makeGuard NoGuard   = "True \n" ++ "   checkGuard _ _ = False \n"
                makeGuard (Guard s) = s ++ "\n   checkGuard _ _ = False \n"
                makeBody :: Body -> String
                makeBody NoBody    = "Just [] \n" ++ "   instBody _ _ = Just [] \n"
                makeBody (Body s)  = "Just [" ++ s ++ "]\n   instBody _ _ = Just [] \n"
                makeBody FalseBody = "Nothing \n" ++ "   instBody _ _ = Nothing \n"
         makePatterns :: String -> [String] -> [(String,Type)] -> Translator String
         makePatterns r (p:ps) venv = do
                outs     <- makePatterns r ps venv
                Just pat <- lookupTransEnv patEnv p
                (pat',eqs) <- makeLinearPat pat []
                let sel = sort (makeSelections pat venv)
                    sub = makeSubstInst sel venv
                    psubs = permutateSubst sub
                    out = makePattern r p sub psubs pat' eqs
                return (out ++ "\n" ++ outs)
         makePatterns _ [] _ = return ""
         makePattern :: String -> String -> Subst -> [(Subst,[EqContext])] -> Pattern -> [EqContext] -> String
         makePattern r p sub ((sub',eqs):env) pat eqs' =
                let mpat  = "   matchPattern (" ++ r ++ " " ++ p ++ ") " ++ (printIt sub') ++ " (" ++ (printIt pat) ++ ")"
                    eqs'' = eqs ++ eqs'
                in case eqs'' of
                     [] -> mpat ++ " = Just " ++ (printIt sub) ++ "\n" ++ (makePattern r p sub env pat eqs')   
                     _  -> mpat ++ " | " ++ (printItD eqs'' "&&") ++ " = Just " ++ (printIt sub) ++ "\n" ++ (makePattern r p sub env pat eqs')   
         makePattern r p _ [] _ _ = "   matchPattern (" ++ r ++ " " ++ p ++ ") _ _ = Nothing \n"

outputCons :: Translator ()
outputCons = do
   name <- getField outputName
   cons <- getField consType
   rs   <- getRules
   let len     = length rs
       postFix = "instance Constraint " ++ cons ++ " where \n"
       output  = postFix ++ outputDeriv1 ++ (outputDeriv2 len) ++ (outputDeriv3 len) ++ "\n"
   lift (lift (appendFile (name ++ ".hs") output))   
   where
      outputDeriv1 = makeSeq 3 ["   derivation _ _ termSig _ [] = doAtomic (writeTVar termSig True)",
                                "derivation (t,i0) sts termSig errSig (ic:ics) = do",
                                "         dontDrop <- doAtomic (validateContent ic)", 
                                "         (if dontDrop then do"]
      outputDeriv2 1 = "\n" ++ (space 15) ++ "(i1,ics1,b1) <- solverThread (t,i0) R1Dummy sts errSig ic False"
      outputDeriv2 i = let is  = show i
                           is' = show (i-1)
                       in (outputDeriv2 (i-1)) ++ "\n" ++ (space 15) ++
                          "(i" ++ is ++ ",ics" ++ is ++ ",b" ++ is ++ ") <- solverThread (t,i" ++ is' ++ ") R" ++ is ++
                          "Dummy sts errSig ic b" ++ is'
      outputDeriv3 i = "\n" ++ (space 15) ++ "let ics' = " ++ (body1 i) ++ "\n" ++ (space 15) ++ 
                       (body2 i) ++ "\n" ++ (space 13) ++ "else derivation (t,i0) sts termSig errSig ics)"
                       where
                          body1 1 = "ics1"
                          body1 i = (body1 (i-1)) ++ " ++ ics" ++ (show i)
                          body2 i = "derivation (t,i" ++ (show i) ++ ") sts termSig errSig (ics ++ ics')"

-------------------------------------------------------
-- Translator Monad Interfaces
-------------------------------------------------------

compileCHR :: Translator ()
compileCHR = do
   -- Preprocessing --
   preprocessModName
   preprocessConsTypes
   preprocessDataDecs
   preprocessCHRRules
   preprocessHaskCodes
   -- Output --
   outputModuleName
   outputHaskCodes
   outputConstrDec
   outputRulePatDec
   outputCHRRules
   outputCons
   printState

runCompiler :: String -> IO ()
runCompiler file = do
   prog <- readFile file
   es   <- runParser prog
   let st = defaultState es
   et <- runErrorT (evalStateT compileCHR st) 
   case et of
     Left  e -> putStrLn (show e)
     Right _ -> putStrLn "Succ"

------------------------------------------------------------------------------
-- Haskell output
------------------------------------------------------------------------------

space :: Int -> String
space i | i < 0 = ""
space 0 = ""
space i = " " ++ (space (i-1))

makeSeq :: Int -> [String] -> String
makeSeq _ []     = ""
makeSeq _ [s]    = s
makeSeq i (s:ss) = s ++ "\n" ++ (space i) ++ (makeSeq i ss)

makeDelimit :: String -> [String] -> String
makeDelimit _ []     = ""
makeDelimit _ [s]    = s
makeDelimit d (s:ss) = s ++ d ++ (makeDelimit d ss)

makeIfElse :: Int -> String -> [String] -> [String] -> String
makeIfElse i g seq1 seq2 = "(if " ++ g ++ " then " ++ (makeSeq (i+5) seq1) ++ "\n" ++ (space (i+1)) ++ 
                           "else " ++ (makeSeq (i+5) seq2) ++ ")"

main :: IO ()
main = do 
   args <- getArgs
   case args of
     [file] -> runCompiler file
     _      -> putStrLn "Usage: runCompiler <FileName>"
