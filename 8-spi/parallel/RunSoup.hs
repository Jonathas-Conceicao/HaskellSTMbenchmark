{-# OPTIONS -fglasgow-exts #-}

module RunSoup( initSoup, runSoup, Exports, extractExports, runProc ) where
                                                  
import Soup
import Chan ( GblId, mkGblId,
	      Chan, Rate, mkChan, chanRate, scaledRate,
	      addRates, infiniteRate, isInfiniteRate, nextUnique )

import qualified Data.Map as Map
import Control.Monad	( foldM )
import System.IO.Unsafe	( unsafePerformIO )
import Data.IORef
import qualified System.Random as Random
import Numeric ( showFFloat)
import Data.STRef
import Control.Monad.ST
import Control.Exception( assert )

-------------------
-- Initialise the soup
initSoup :: [(Int,Proc)] -> Soup -> Soup
initSoup init_procs = initM_ init_soup
  where
    init_soup soup = do { soup' <- foldM init_proc soup init_procs
			; return soup' }
    init_proc soup (n,p) = do { ps <- runProc emptyEnv p
                              ; return (extendSoup [(p,n) | p<- ps] soup) }

-------------------
-- Run the soup for a specified interval
runSoup :: Time 	-- Run for this interval
	-> Soup
	-> Soup
runSoup interval soup 
  = initM_ runSoupM (bumpDeadline interval soup)

runSoupM :: Soup -> M Soup
runSoupM soup 
  = if (soup_time soup) > (soup_deadline soup)
    then return soup
    else do  { soup'maybe <- step soup
             ; case soup'maybe of 
                 Nothing    -> return soup
                 Just soup' -> runSoupM soup' }

-------------------
-- Returns Nothing if the soup's run queue is empty; otherwise takes a step
step :: Soup -> M (Maybe Soup)
step soup
  = do	{ debug soup (show soup)
        -- ; debug soup ("Step time " ++ showFFloat (Just 10) (soup_time soup) "")

        ; let weights  = findWeights soup
 	      activity = addRates (map snd weights)
 	      infinite_rate = isInfiniteRate activity

	-- ; debug soup ("weights: " ++ showFFloat (Just 2) activity (" " ++ show weights))
        -- If there are no processes to run, stop here
        ; if weights == [] then 
             do { debug soup "Empty run queue"
                ; return Nothing }
          else do 

	-- Choose a random number in 0..activity
	-- But if the activity is infinity, choose infinity
 	{ w <- if infinite_rate
	       then return infiniteRate
	       else randomDouble activity

	; let choice = choose w weights
	      ready_sets = readySets soup choice

	; (ps_in, ps_out) <- stepChoice soup choice ready_sets

	; t <- randomDouble 1
	; let delta_t | infinite_rate = 0
		      | otherwise     = (1/activity) * log (1/t)
              new_soup = extendSoup [(p,1) | p <- ps_out]	$
			 delProcsFromSoup ps_in			$
			 bumpTime delta_t soup

	; debug soup (showFFloat (Just 7) (soup_time new_soup) (": " ++ 
     		      show ps_in ++ "  ==>  " ++ show ps_out))

	; return (Just new_soup)
	}}

findWeights :: Soup -> [(Chan, Rate)]
findWeights soup = [ (chan, scaledRate chan (soup_ncells soup) act)
		   | (chan, set) <- soupReadySets soup,
		     let act = readySetActivity set,
		     act > 0 ]

---------------------
choose:: (Ord w, Num w, Show w, Show a) => w -> [(a,w)] -> a
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
	; new_ps <- stepDelay proc c
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
		= (mix_proc, delFromProcSet (mix_proc,1) mixps)
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

stepDelay :: QProc -> Chan -> M [QProc]
stepDelay (QProc fun args) chan
  = do	{ let env1 = mkEnv (qa_params fun) args
	; env2 <- newChans env1 (qa_new fun)
	; let proc = head [p | (Delay r, p) <- qa_alts fun, 
				chanRate chan == lookupLit env2 r]
		-- The process could be waiting for more than one
		-- delay; choose the one that has fired
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


type Exports = [QProcs]

extractExports	:: Int		-- Number of neighbours
	 	-> Float	-- Fraction to export
		-> Soup
		-> (Soup, [Exports])	-- Exactly as many exports as neighbours

-- Identify things to export, remove them from the soup, 
-- and return them
extractExports n_neighbours export_fraction soup
  | n_neighbours == 0 = (soup, [])
  | otherwise
  = initM extractM soup
  where
    extractM soup 
	= do { emigrants <- find_emigrants (soupProcs soup)
	     ; exports <- distribute emigrants
	     ; let soup' = foldr delFromSoup soup emigrants
	     ; assert (sum [n | (p,n) <- emigrants] == sum [n | (p,n) <- concat exports]) $
	       return (soup', exports) }

    find_emigrants :: [QProcs] -> M Exports
    find_emigrants procs = mapM find_one procs
    find_one :: QProcs -> M QProcs
    find_one (proc, n) = do { rands {- :: [Float] -} <- randomRsM (0,1)
			    ; return (proc, length $ filter (< export_fraction) $
					    take n rands) }

    distribute :: Exports -> M [Exports]
	-- Result is a partition of the input
    distribute emigs
	= do { exp_s <- mapM dist_one emigs
	     ; return (Map.elems (foldr (Map.unionWith (++)) Map.empty exp_s)) }

    dist_one :: QProcs -> M (Map.Map Int Exports)
	-- The exports for *one particular* QProc
    dist_one (p,n) = do { rands {- :: [Int] -} <- randomRsM (1,n_neighbours)
			; let hist :: Map.Map Int Int
			      hist = histogram (take n rands)
			; return (Map.map (\n -> [(p,n)]) hist) }


histogram :: [Int] -> Map.Map Int Int
histogram []     = Map.empty
histogram (n:ns) = Map.insertWith (+) n 1 (histogram ns)

-------------------------------------------
--	The run monad, M
-------------------------------------------

-- The monad is simply there to hide the plumbing of 
-- the unique suppply and random number generator
-- It is only used in this module

newtype M a = M { unM :: forall s. StateEnv s -> ST s a }
	-- Just for fun, use runST to encapsulate!

data StateEnv s = SE { se_ns  :: STRef s Unique, 		-- Name supply
		       se_rng :: STRef s Random.StdGen }	-- Random numers

initM_ :: (Soup -> M Soup) -> Soup -> Soup
initM_ do_it soup = fst (initM (\s -> do { s' <- do_it s; return (s',()) }) soup)

initM :: (Soup -> M (Soup,a)) -> Soup -> (Soup, a)
initM do_it soup
  = runST (do 
	{ us  <- newSTRef (soup_uniqs soup)
	; rng <- newSTRef (soup_rng soup)
	; (soup', res) <- unM (do_it soup) (SE {se_ns = us, se_rng = rng})
	; us' <- readSTRef us
	; rng' <- readSTRef rng
	; return (soup' { soup_uniqs = us', soup_rng = rng' }, res)
    })

instance Monad M where
  return x  = M (\env -> return x)
  M m >>= k = M (\env -> do { r <- m env; unM (k r) env })

--------------------
-- Debugging

mTrace :: IO a -> M ()
mTrace io = unsafePerformIO io `seq` return ()

debug soup s =
  if debugOn then 
    mTrace (putStrLn ("[" ++ (show (soup_id soup)) ++ "] " ++ s))
  else
    return ()

--------------------
-- Random numbers

randomRM :: Random.Random a => (a,a) -> M a
randomRM range = M (\env -> do	{ rng <- readSTRef (se_rng env)
				; let (v, rng') = Random.randomR range rng
				; writeSTRef (se_rng env) rng'
				; return v })

randomRsM :: Random.Random a => (a,a) -> M [a]	-- Like Random.randomRs
randomRsM range 
  = M (\env -> do { rng <- readSTRef (se_rng env)
		  ; let (g1,g2) = Random.split rng
		  ; writeSTRef (se_rng env) g2
		  ; return (Random.randomRs range g1) })

randomInt :: Int -> M Int
-- (randomInt n) produces an Int in the range [1..n]
randomInt n = randomRM (1, n)

randomDouble :: Double -> M Double
-- (randomDouble f) produces an Double in the range [0..f]
randomDouble f = randomRM (0,f)

--------------------
-- Uniques

newUnique :: M Unique
newUnique = M (\env -> do { let ns = se_ns env
			  ; u <- readSTRef ns
			  ; writeSTRef ns (nextUnique u 1)
			  ; return u })

newGId :: String -> M GblId
newGId s = do { u <- newUnique; return (mkGblId u s) }

newChan :: Env -> NewChanBndr -> M Env
newChan env (lid@(LclId _ str), rate) 
  = do	{ gid <- newGId str
	; let chan = mkChan gid (lookupLit env rate)
	; return (extendEnv "newChan" env [lid] [GChan chan]) }

newChans :: Env -> [NewChanBndr] -> M Env
newChans env bndrs = foldM newChan env bndrs
