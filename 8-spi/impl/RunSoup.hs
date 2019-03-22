module RunSoup( runSoup, initM ) where

import Soup
import Chan	( GblId, mkGblId,
		  Chan, Rate, mkChan, chanRate,
		  scaleRate, addRates, infiniteRate, isInfiniteRate )
import qualified Data.Map as Map
import Control.Monad	( foldM )
import System.Random	( randomRIO )
import Data.IORef

-------------------------------------
runSoup :: Soup -> [(Int, Proc)] -> M ()
runSoup init_soup [] = error "Nothing to run"

runSoup init_soup init_procs
  = do	{ soup <- foldM init_proc init_soup init_procs
		-- Put the initial processes in the soup
	; mIO (showDebugHeaders soup)
	; mIO (showHeaders soup)

	; loop soup
	}
  where
    init_proc soup (n,p) = do { ps <- runProc emptyEnv p
			      ; return (extendSoup n ps soup) } 

loop :: Soup -> M ()
loop soup 
  = do  { mb_soup <- logSoup soup
	; case mb_soup of
	     Nothing -> return ()
	     Just soup' -> do { soup'' <- step soup'
			      ; loop soup'' } }

-------------------
logSoup :: Soup -> M (Maybe Soup)	-- Nothing <=> fnished
logSoup soup
  | cur_time >= sl_finish log_info
  = return Nothing
  | cur_time >= sl_log_time log_info
  = do { mIO (showDebug soup)
       ; mIO (showPops soup)
       ; return (Just (soup { soup_log = log_info' })) }
  | otherwise
  = return (Just soup)
  where
    cur_time = soup_time soup
    log_info = soup_log soup
    max_logs = sl_max_logs log_info
    log_info' = log_info {sl_logs = new_n, sl_log_time = new_lt}
      
    (new_n,new_lt) = find_next (sl_logs log_info + 1)

    find_next n | cur_time >= lt = find_next (n+1) 
		| otherwise      = (n,lt)
	where
	  lt = sl_finish log_info * fromIntegral (n+1) / fromIntegral max_logs

-------------------
step :: Soup -> M Soup
step soup
  = do	{ let weights  = findWeights soup
	      activity = addRates (map snd weights)
	      infinite_rate = isInfiniteRate activity

	-- Choose a random number in 0..activity
	-- But if the activity is infinity, choose infinity
 	; w <- if infinite_rate
	       then return infiniteRate
	       else randomDouble activity

-- 	; mIO (putStrLn (show weights))
	; let choice = choose w weights
	      ready_sets = readySets soup choice

	; (ps_in, ps_out) <- stepChoice soup choice ready_sets

	; t <- randomDouble 1
	; let delta_t | infinite_rate = 0
		      | otherwise     = (1/activity) * log (1/t)
              new_soup = extendSoup 1 ps_out	$
			 delFromSoup ps_in	$
			 bumpTime delta_t soup

--	; mIO (putStrLn ("Weights: " ++ show activity ++ " " ++ show weights))
--	; mIO (putStrLn $
--		show (soup_time new_soup) ++ ": " ++ 
--		show ps_in ++ "  ==>  " ++ show ps_out)
--	; mIO (putStrLn (show (Map.toList (soup_procs soup))))

	; return new_soup
	}

findWeights :: Soup -> [(Chan, Rate)]
findWeights soup = [ (chan, scaleRate act (chanRate chan))
		   | (chan, set) <- Map.toList (soup_chans soup),
		     let act = readySetActivity set,
		     act > 0 ]

---------------------
choose :: (Ord w, Num w, Show w, Show a) => w -> [(a,w)] -> a
-- (choose b [...(ai,wi)...]) returns the first 'ai' 
-- such that the sum of the weights wi 
-- of that item and the previous ones is >= b
choose orig_b orig_cws 
  = loop orig_b orig_cws
  where
    loop b ((c,w):cws) | w >= b    = c
 		       | otherwise = choose (b-w) cws
    loop b [] = error ("choose " ++ show orig_b ++ " " ++ show orig_cws)


--------------------------------------
stepChoice :: Soup -> Chan -> ReadySets -> M ([QProc], [QProc])
stepChoice soup c (DelayRS set)
  = do	{ index <- randomInt (procSetSize set)
	; let proc = choose index (procSetItems set)
	; new_ps <- stepDelay proc (chanRate c)
	; return ([proc], new_ps) }

stepChoice soup c (ChanRS { in_procs = inps, out_procs = outps, mix_procs = mixps })
  = do	{ let n_mixps = procSetSize mixps
	
	-- Choose which process will input on c
	; in_index <- randomInt (n_mixps + procSetSize inps)
		-- in_index starts at 1
	; let (in_proc, mixps') 
		| in_index > n_mixps 	-- Choose from inps
		= (choose (in_index - n_mixps) (procSetItems inps), mixps)
		| otherwise	       	-- Choose from mixps
		= (mix_proc, delFromProcSet mix_proc mixps)
	      mix_proc = choose in_index (procSetItems mixps)
		
	-- Choose which process will output on c
	; out_index <- randomInt (procSetSize mixps' + procSetSize outps)
	; let out_proc = choose out_index (procSetItems mixps' ++ procSetItems outps)

	-- Run both processes one step
	; (new_ps1, outcs) <- stepOut out_proc c 
	; new_ps2 <- stepIn in_proc c outcs

	; return ([out_proc, in_proc], new_ps1 ++ new_ps2)
	}

--------------------------------------
stepOut :: QProc -> Chan -> M ([QProc], [GVal])
stepOut (QProc fun args) chan 
  = do	{ let env1 = mkEnv (qa_params fun) args
	; env2 <- newChans env1 (qa_new fun)
	; let (outs, proc) = head [(cs,p) | (Out c cs, p) <- qa_alts fun,
				 	    chan == lookupChan env2 c]
	; ps <- runProc env2 proc
	; return (ps, map (lookupVal env2) outs) }

stepIn :: QProc -> Chan -> [GVal] -> M [QProc]
stepIn (QProc fun args) chan inps
  = do	{ let env1 = mkEnv (qa_params fun) args
	; env2 <- newChans env1 (qa_new fun)
	; let (bndrs, proc) = head [(bs,p) | (In c bs, p) <- qa_alts fun,
				 	     chan == lookupChan env2 c]
	      env3 = extendEnv "stepIn2" env2 bndrs inps
	; runProc env3 proc }

stepDelay :: QProc -> Rate -> M [QProc]
stepDelay (QProc fun args) rate
  = do	{ let env1 = mkEnv (qa_params fun) args
	; env2 <- newChans env1 (qa_new fun)
	; let proc = head [p | (Delay r, p) <- qa_alts fun, 
				rate == lookupLit env2 r]
	; runProc env2 proc }


-------------------------------
runProc :: Env -> Proc -> M [QProc]
runProc env (Par ps) 	 = do { pss <- mapM (runProc env) ps
		 	      ; return (concat pss) }
runProc env (New bndr p) = do { env1 <- newChan env bndr
			      ; runProc env1 p }

runProc env (Call fun args) 
  = case lookupVal env fun of
	GAbs fun   -> return [QProc fun (lookupVals env args)]
	GMacro mac -> runMacro mac (lookupVals env args)
	other      -> unexpected "abstraction" fun

runMacro :: Macro -> [GVal] -> M [QProc]
runMacro (Macro _ params rhs) args
  = runProc (mkEnv params args) rhs


unexpected :: String -> Occ -> a
unexpected what gval
  = error ("Expecting " ++ what ++ ", but found " ++ ppr gval)
  where
    ppr (LclOcc i) = show i
    ppr (GblOcc g) = ppr_gv g
    ppr_gv (GAbs f)   = "an abstraction " ++ show f
    ppr_gv (GMacro m) = "an abstraction " ++ show m
    ppr_gv (GChan c)  = "a channel " ++ show c
    ppr_bv (GLit f)   = "a literal " ++ show f


-------------------------------------------
--	The run monad, M
-------------------------------------------

-- The monad is the IO monad, plus an environment to 
-- carry the main state refs

newtype M a = M { unM :: StateEnv -> IO a }

data StateEnv = SE { se_ns :: IORef Unique }	-- Name supply

initM :: Unique -> M a -> IO a
initM uniq (M m) = do { ns <- newIORef uniq
	              ; m (SE { se_ns = ns }) }

instance Monad M where
  return x  = M (\env -> return x)
  M m >>= k = M (\env -> do { r <- m env; unM (k r) env })

mIO :: IO a -> M a
mIO io = M (\env -> io)

randomInt :: Int -> M Int
-- (randomInt n) produces an Int in the range [1..n]
randomInt n = mIO (randomRIO (1, n))

randomDouble :: Double -> M Double
-- (randomDouble f) produces an Double in the range [0..f]
randomDouble f = mIO (randomRIO (0, f))

newUnique :: M Unique
newUnique = M (\env -> do { let ns = se_ns env
			  ; u <- readIORef ns
			  ; writeIORef ns (u+1)
			  ; return u })

newGId :: String -> M GblId
newGId s = do { u <- newUnique; return (mkGblId u s) }

makeChan :: String -> Rate -> M Chan
makeChan s rate = do { gid <- newGId s; return (mkChan gid rate) }

newChan :: Env -> NewChanBndr -> M Env
newChan env (lid@(LclId _ str), rate) 
  = do	{ gid <- newGId str
	; let chan = mkChan gid (lookupLit env rate)
	; return (extendEnv "newChan" env [lid] [GChan chan]) }

newChans :: Env -> [NewChanBndr] -> M Env
newChans env bndrs = foldM newChan env bndrs
