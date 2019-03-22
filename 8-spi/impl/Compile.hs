module Compile where

import Soup
import Chan	( Chan, mkChan, GblId, mkDelayChan, mkGblId, firstUnique )
import Parser
import qualified Data.Map as Map

---------------------------
type ProgInfo = ((Time,Int),		-- Sampling info
		[PlotItem QAbs Chan],	-- Plotting info
		[(Int,Proc)])		-- Initial processes

compileProgram :: [Decl] 
	       -> (ProgInfo,		-- Initial setup
		   Unique)		-- Next free unique (sigh)

compileProgram ds
  = (initCM init_cxt (compileDirectives ds), 
     length binders + 1)
  where
    binders = findBinders ds

    gid_env :: IdEnv
    gid_env = Map.fromList $
	      [ (s, Gbl (mkGblId u s))
	      | (u,s) <- [firstUnique..] `zip` binders]

    gbl_env  = initCM init_cxt (compileDecls ds)
    init_cxt = initCxt gid_env gbl_env

---------------------------
compileDirectives :: [Decl] -> CM ProgInfo
compileDirectives ds 
  = do  { plots <- mapM do_plot (filter isPlotDecl ds)
	; runs  <- mapM do_run (filter isRunDecl ds)
	; let samples = case [(t,n) | Sample t n <- ds] of
			  [] 	 -> (1,100)
			  (tn:_) -> tn
	; return (samples, concat plots, runs) }
  where
    do_plot (Plot is) = mapM do_plot_item is
    do_plot_item (PlotProc fn s) 
	= do { Gbl gid <- lookupName fn
	     ; gval <- lookupGId gid
	     ; return (PlotProc (unGAbs gval) s) }
    do_plot_item (PlotChan ch mode s)
	= do { Gbl gid <- lookupName ch
	     ; gval <- lookupGId gid
	     ; return (PlotChan (unGChan gval) mode s) }

    do_run (Run n p) = do { p' <- compileProc p; return (n,p') }

---------------------------
compileDecls :: [Decl] -> CM GblEnv
compileDecls ds
  = do	{ prs <- mapM compileDecl (filter isValDecl ds)
	; return (Map.fromList prs) }

compileDecl :: Decl -> CM (GblId, GVal)
compileDecl (NewDecl s rate _)
  = do	{ Gbl gid <- lookupName s
	; rate' <- compileTopVal rate
	; return (gid, GChan (mkChan gid rate')) }

compileDecl (ValDecl s rhs)
  = do 	{ Gbl gid <- lookupName s
	; val <- compileTopVal rhs
	; return (gid, GLit val) }

compileDecl (LetDecl fn pats rhs)
  = cmWrap ("the right hand side of " ++ fn) $
    extendLcls pats $ \ params -> 
    do  { Gbl gid <- lookupName fn
	; if isMacro rhs then 	-- A macro
		do { body <- compileProc rhs
		   ; return (gid, GMacro (Macro gid params body)) }
	  else			-- An abstraction
		do { (news, alts) <- compileRhs rhs
		   ; let abs = QAbs { qa_id = gid,
			   	      qa_params = params,
			   	      qa_new = news,
			   	      qa_alts = alts,
			   	      qa_gates = findGates alts }
		   ; return (gid, GAbs abs) }
	}

isMacro :: PProc -> Bool
isMacro (PDecl (NewDecl _ _ _) p) = isMacro p
isMacro (PDo _)    = False
isMacro (PPar [p]) = isMacro p
isMacro other      = True

findGates :: [(Guard,proc)] -> [Gate Occ]
findGates alts
  = Map.foldWithKey mk_chan_gate delay_gates chan_modes
  where
    delay_gates = [Gate DelayMode r | (Delay r, _) <- alts]
    mk_chan_gate chan mode rest = Gate mode chan : rest

    chan_modes :: Map.Map ChanOcc Mode
    chan_modes = foldl add Map.empty alts
    add fm (In c _,  _) = Map.insertWith combine c InMode fm
    add fm (Out c _, _) = Map.insertWith combine c OutMode fm
    add fm (Delay _, _) = fm
    
    combine InMode InMode   = InMode
    combine OutMode OutMode = OutMode
    combine _	    _ 	    = MixMode

-------------------------
compileRhs :: PProc -> CM ([NewChanBndr], [(Guard,Proc)])
compileRhs (PDecl (NewDecl s r _) p)
  = extendLcls [s] $ \ [lcl] ->
    do 	{ (news, alts) <- compileRhs p 
	; r' <- compileVal r
	; return ((lcl,r') : news, alts) }

compileRhs (PDo alts)
  = do	{ alts' <- mapM compileAlt alts
	; return ([], alts') }

compileRhs p = fail ("Illegal rhs: "++ show p)
	-- E.g. a non-choice in the RHS of a proc

compileAlt :: (Action,PProc) -> CM (Guard,Proc)
compileAlt (InAct c vs, p) 
  = do  { c' <- lookupOcc c; 
	; extendLcls vs $ \ vs' -> do
	{ p' <- compileProc p
	; return (In c' vs', p') }}

compileAlt (OutAct c vs, p) 
  = do  { c' <- lookupOcc c; 
	; vs' <- compileArgs vs
	; p'  <- compileProc p
	; return (Out c' vs', p') }

compileAlt (DelayAct r, p) 
  = do	{ r' <- compileVal r
	; p'  <- compileProc p
	; return (Delay r', p') }

------------------------------------
-- Compiling the branch of an alternative

compileProc :: PProc -> CM Proc
compileProc (PPar ps)
  = do	{ ps' <- mapM compileProc ps ; return (Par ps') }

compileProc p@(PCall fn vals)
  = cmWrap (show p) $
    do	{ id <- lookupName fn
	; vals' <- compileArgs vals
	; fn' <- case id of
		    Gbl gid -> do { gval <- lookupGId gid; return (GblOcc gval) }
		    Lcl lid -> return (LclOcc lid)
	; return (Call fn' vals')
	}

compileProc (PDecl (NewDecl c r _) p)
  = extendLcls [c] $ \ [lcl] ->
    do	{ p' <- compileProc p
	; r' <- compileVal r
	; return (New (lcl,r') p') }

compileProc p = fail ("Illegal proc: "++ show p)
	-- E.g. a choice in the RHS of a choice


------------------------------------
-- Compiling a value declaration

compileTopVal :: PValue -> CM Double
-- This one must be a literal value, not a LclOcc
compileTopVal (LitVal lit) = return lit
compileTopVal (IdVal s)    
  = do	{ Gbl gid <- lookupName s
	; gval <- lookupGId gid; 
	; return (unGLit gval) }

compileVal :: PValue -> CM Occ
compileVal (LitVal lit) 
  = return (GblOcc (GLit lit))
compileVal (IdVal s)    
  = do	{ id <- lookupName s
	; case id of
	    Gbl gid -> do { gval <- lookupGId gid; return (GblOcc gval) }
	    Lcl lid -> return (LclOcc lid)
	}

unexpected :: String -> String -> CM a
unexpected what found 
  = fail ("Expecting " ++ what ++ " but found " ++ found)

---------------------------
findBinders :: [Decl] -> [String]
-- Find the top-level binders
findBinders ds = concatMap find ds
  where
    find (NewDecl s _ _)   = [s]
    find (LetDecl s _ rhs) = [s]
    find (ValDecl s _)     = [s]
    find other 		   = []


--------------------------------------
--	Compilations environment
--------------------------------------

data CmCtxt = CmCtxt { 
	cm_fn    :: String,	-- RHS of this decl
	cm_stack :: [String],	-- Stack of things we are inside
				-- (for error reporting)
	cm_idenv  :: IdEnv,	-- Binds srings to their unique Id
	cm_gblenv :: GblEnv,	-- Knot-tying env for globals
	cm_uniq   :: Unique	-- Next available local unique
		}

initCxt idenv gblenv
  = CmCtxt { cm_fn = "Top level", cm_stack = [],
	     cm_gblenv = gblenv, cm_idenv = idenv,
	     cm_uniq = 1 }

-------------------
-- 	Environments
-------------------

type IdEnv  = Map.Map String Id
data Id = Gbl GblId	-- Global value
	| Lcl LclId	-- Local thing (of unknown type)
	deriving( Show )

type GblEnv = Map.Map GblId GVal

extendLcls :: [String] -> ([LclId] -> CM a) -> CM a
extendLcls ss thing_inside
  = updCxt upd thing_inside
  where
    upd cxt = (cxt', lcls)
	where
	   cxt' = cxt { cm_idenv = foldl add (cm_idenv cxt) lcls, 
		  	cm_uniq = uniq + length ss }
	   uniq = cm_uniq cxt
	   lcls = [LclId u s | (u,s) <- [uniq..] `zip` ss]

    add env lid@(LclId _ s) = Map.insert s (Lcl lid) env

lookupName :: String -> CM Id
lookupName s
  = do  { cxt <- getCxt
	; case Map.lookup s (cm_idenv cxt) of
	    Just id -> return id
	    other -> fail (s ++ " is not in scope" ++ show (Map.toList (cm_idenv cxt)))
	}

lookupGId :: GblId -> CM GVal
lookupGId gid
  = do  { cxt <- getCxt		-- NB: return lazy lookup thunk
	; return (case Map.lookup gid (cm_gblenv cxt) of
		    Just id -> id
		    other   -> error ("Panic: lookupGId " ++ (show gid)))
	}

compileArgs :: [PValue] -> CM [ChanOcc]
compileArgs vs = mapM do_one vs
  where
    do_one (IdVal s)  = lookupOcc s
    do_one (LitVal n) = error "Literal found where channel expected"

lookupOcc :: String -> CM Occ
lookupOcc s
  = do	{ id <- lookupName s
	; case id of
	    Lcl lid -> return (LclOcc lid)
	    Gbl gid -> do { gval <- lookupGId gid
			  ; return (GblOcc gval) }
	}
		

---------------------------
newtype CM a = CM { unCM :: CmCtxt -> a }

instance Monad CM where
  return x = CM (\cxt -> x)
  (CM m) >>= k = CM (\cxt -> unCM (k (m cxt)) cxt)
  fail s = CM (\cxt -> error (s ++ showStack (cm_stack cxt)))

initCM :: CmCtxt -> CM a -> a
initCM cxt (CM m) = m cxt

cmWrap :: String -> CM a -> CM a
cmWrap s thing_inside 
  = CM (\cxt -> unCM thing_inside (cxt { cm_stack = s : cm_stack cxt }))

showStack :: [String] -> String
showStack ss = concat (map ("\nIn: " ++) ss)

getCxt :: CM CmCtxt
getCxt = CM (\env -> env) 

updCxt :: (CmCtxt -> (CmCtxt, a)) -> (a -> CM b) -> CM b
updCxt upd thing_inside
  = CM  (\cxt -> let (cxt', a) = upd cxt
		in unCM (thing_inside a) cxt'
	)

