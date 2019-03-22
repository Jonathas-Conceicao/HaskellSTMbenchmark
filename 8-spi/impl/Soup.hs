module Soup( 
	Soup(..), SoupLog(..), PlotItem(..),
	Guard(..), Proc(..), 
	QAbs(..), QProc(..), Mode(..), Gate(..),
	GblId, LclId(..), Unique,
	Macro(..), NewChanBndr,
	ChanOcc, Occ(..), GVal(..), 
	Weight, Time,

	ProcSet, procSetSize, procSetItems, delFromProcSet, 
	ReadySets(..), readySetActivity,

	emptySoup, extendSoup, delFromSoup, bumpTime,
	readySets, showHeaders, showPops, 
	showDebugHeaders, showDebug,

	Env, emptyEnv, mkEnv, extendEnv, 
	lookupChan, lookupLit, lookupVal, lookupVals, 
	
  ) where

import Chan( Chan, mkDelayChan, Rate, GblId, Unique ) 
import qualified Data.Map as Map
import Data.Maybe	( fromJust )
import System.IO	( Handle, hPutStrLn, stderr )
import Data.List	( intersperse, partition )


debugOn = False

-------------------------------------
--	The Soup type and friends
-------------------------------------

data Soup = Soup { 
	soup_time  :: Time,	-- Current time, starts at zero
	soup_steps :: Int,	-- Number of steps
	soup_log   :: SoupLog,
			
	soup_chans  :: Map.Map Chan ReadySets,

	soup_procs :: Map.Map QAbs Int }
		-- Initialised with the QAbs's we want to track
		-- (the others are ignored)

data SoupLog = SoupLog {
	sl_handle   :: Handle,
	sl_finish   :: Time,	-- Finish time
	sl_max_logs :: Int,	-- Number of desired log lines

	sl_log_time :: Time,	-- Time for next log line
	sl_logs     :: Int,	-- Number of log lines so far (starts zero)

	sl_plot :: [PlotItem QAbs Chan],	-- What to plot

	sl_debug :: Bool	-- True <=> output debugging info too
  }
			
data PlotItem proc chan 
  = PlotProc proc String | PlotChan chan Mode String

-- Activity = sum of all the rates of all the processes

emptySoup :: Handle -> (Time,Int) -> [PlotItem QAbs Chan] -> Soup
emptySoup log_handle (finish, max_logs) items 
  = Soup { soup_time = 0, soup_steps = 0,
	   soup_log  = SoupLog { sl_handle = log_handle,
				 sl_finish = finish, sl_max_logs = max_logs,
				 sl_log_time = 0, sl_logs = 0,
				 sl_plot = items,
				 sl_debug = debugOn },	-- Improve!
	   soup_chans = Map.empty, 

	   soup_procs = Map.fromList [(fn,0) | PlotProc fn _ <- items] }

bumpTime :: Time -> Soup -> Soup
bumpTime delta_t soup = soup { soup_time = soup_time soup + delta_t,
			       soup_steps = soup_steps soup + 1 }

trackAllProcs = False

extendSoup :: Int -> [QProc] -> Soup -> Soup
-- Add n times the procs to the soup
extendSoup n ps soup 
  = foldr add soup ps
  where
    add p soup = foldr (add_gate p) (add_to_procs p soup) (gates p)

    add_to_procs p soup@(Soup { soup_procs = procs })
 	= soup { soup_procs = Map.adjust (+ n) (procAbs p) procs' }
	where
	  fn = procAbs p
	  procs' | trackAllProcs && not (fn `Map.member` procs)
		 = Map.insert fn 0 procs
		 | otherwise
		 = procs
			-- NB: If the qabs isn't already in the soup_procs,
			--     this adjust does nothing.  The idea is that
			--     we are only keeping population counts for some
			--     abstractions, the ones we were asked to log

    add_gate :: QProc -> Gate Chan -> Soup -> Soup
    add_gate p (Gate mode chan) soup@(Soup { soup_chans = chans })
	= soup { soup_chans = Map.adjust (updateRS mode (extendProcSet n p)) 
					 chan chans' }
	where
	  chans' | chan `Map.member` chans = chans
		 | otherwise = Map.insert chan (emptyRS mode) chans

delFromSoup :: [QProc] -> Soup -> Soup
delFromSoup ps soup 
  = foldr del soup ps
  where
    del p soup = foldr (del_gate p) (del_from_procs p soup) (gates p)
    del_from_procs p soup@(Soup { soup_procs = procs })
	= soup { soup_procs = Map.adjust (\n -> n-1) (procAbs p) procs }

    del_gate p (Gate mode chan) soup@(Soup { soup_chans = chans })
	= soup { soup_chans = Map.update upd chan chans }
	where
	-- Be sure to delete channels that no one is waiting on any more
	-- else the data structure fills up with zombie channels
	  upd rs | isEmptyRS rs' = Nothing	
		 | otherwise     = Just rs'
		 where
		   rs' = updateRS mode (delFromProcSet p) rs

-------------------------------------
--	ReadySets
-------------------------------------

data ReadySets 
  = ChanRS { in_procs, out_procs, mix_procs :: ProcSet }
		-- Processes ready to:
		--	input only, 
		--	output only,
		-- 	or input *and* output
		-- on a channel
		-- A process appears in just one of these three

  | DelayRS { del_procs :: ProcSet }	-- Processes delayed 

emptyRS :: Mode -> ReadySets
emptyRS DelayMode = DelayRS { del_procs = emptyProcSet }
emptyRS other     = ChanRS  { in_procs = emptyProcSet, 
			      out_procs = emptyProcSet, 
			      mix_procs = emptyProcSet }
	
readySets :: Soup -> Chan -> ReadySets
readySets soup chan = case Map.lookup chan (soup_chans soup) of
			    Just rs -> rs
			    Nothing -> error ("chanReadySets" ++ show chan)

updateRS :: Mode -> (ProcSet -> ProcSet) -> ReadySets -> ReadySets
updateRS mode upd (DelayRS procs) = DelayRS (upd procs)
updateRS mode upd rs@(ChanRS {})  = case mode of
					InMode  -> rs { in_procs  = upd (in_procs rs) }
					OutMode -> rs { out_procs = upd (out_procs rs) }
					MixMode -> rs { mix_procs = upd (mix_procs rs) }

readySetActivity :: ReadySets -> Int
readySetActivity (DelayRS ps) 
  = procSetSize ps
readySetActivity (ChanRS { in_procs = ins, out_procs = outs, mix_procs = mixs })
  = (n_ins + n_mixs) * (n_outs + n_mixs) - n_mixs
  where
    n_ins  = procSetSize ins
    n_outs = procSetSize outs
    n_mixs = procSetSize mixs
	
isEmptyRS :: ReadySets -> Bool
isEmptyRS (DelayRS ps) = isEmptyProcSet ps
isEmptyRS (ChanRS { in_procs = ins, out_procs = outs, mix_procs = mixs })
  = isEmptyProcSet ins && isEmptyProcSet outs && isEmptyProcSet mixs

readySetCount :: ReadySets -> Int
readySetCount (DelayRS ps) 
  = procSetSize ps
readySetCount (ChanRS { in_procs = ins, out_procs = outs, mix_procs = mixs })
  = procSetSize ins + procSetSize outs + procSetSize mixs
	
-------------------------------------
--	ProcSet
-------------------------------------

data ProcSet = ProcSet 
		  Int 			-- Number of elements
		  (Map.Map QProc Int)	-- Contents, with number of copies of each

emptyProcSet = ProcSet 0 Map.empty

procSetSize :: ProcSet -> Int
procSetSize (ProcSet n _) = n

isEmptyProcSet :: ProcSet -> Bool
isEmptyProcSet (ProcSet n _) = n==0

procSetItems :: ProcSet -> [(QProc, Int)]
procSetItems (ProcSet _ ps) = Map.toList ps

delFromProcSet :: QProc -> ProcSet -> ProcSet
-- Invariant: the proc is currently in the set
delFromProcSet p (ProcSet n mp) 
  = ProcSet (n-1) (Map.updateWithKey upd p mp)
  where
    upd p 1 = Nothing
    upd p n = Just (n-1)

extendProcSet :: Int -> QProc -> ProcSet -> ProcSet
extendProcSet n p (ProcSet k mp)
  = ProcSet (k+n) (Map.insertWith (+) p n mp)



--------------------------------------
-- Channels
type Time = Double
type Activity = Double
type Weight = Double	-- Rate * Activity

-- Processes
-- Question: can a single choice have two identical guards?

data QProc 	-- A quiescent process application
  = QProc QAbs [GVal]
  deriving( Eq, Ord )

procAbs :: QProc -> QAbs
procAbs (QProc fn _) = fn

data QAbs	-- A quiescent process abstraction
  = QAbs {
	qa_id     :: GblId,		-- Id of the abstraction
	qa_params :: [LclId], 		-- Parameters
	qa_new    :: [NewChanBndr],	-- New channels
	qa_alts   :: [(Guard, Proc)],	-- Choices
	qa_gates  :: [Gate Occ]		-- Cached: the gates for this abstraction
					-- 	Avoids recompuation of InOutMode
    }

data Guard = In ChanOcc [LclId]
	   | Out ChanOcc [ChanOcc]
	   | Delay RateOcc

type NewChanBndr = (LclId, RateOcc)

data Macro = Macro GblId [LclId] Proc

data Proc 
  = Par [Proc]
  | New NewChanBndr Proc
  | Call AbsOcc [Occ]

type ChanOcc = Occ	-- This should be a channel
type RateOcc = Occ	-- ...a rate
type AbsOcc  = Occ	-- ...an abstraction

data Occ = LclOcc LclId 	-- Bound locally
	 | GblOcc GVal		-- Bound globally
	 deriving( Eq, Ord, Show )

data GVal = GAbs   { unGAbs   :: QAbs }
	  | GMacro { unGMacro :: Macro }
	  | GChan  { unGChan  :: Chan }
	  | GLit   { unGLit   :: Double }
	  deriving( Eq, Ord )

data Gate chan = Gate Mode chan
		
data Mode = InMode | OutMode | MixMode | DelayMode 
	  deriving( Show )

gates :: QProc -> [Gate Chan]
gates (QProc fn args)
  = map inst_gate (qa_gates fn)
  where
    env = mkEnv (qa_params fn) args
    inst_gate (Gate DelayMode r) 
	= Gate DelayMode (mkDelayChan (lookupLit env r))
    inst_gate (Gate m c) 
	= Gate m (lookupChan env c)

-------------------------------

type Env = Map.Map LclId GVal

emptyEnv :: Env
emptyEnv = Map.empty

lookupVals :: Env -> [Occ] -> [GVal]
lookupVals env occs = map (lookupVal env) occs

lookupVal :: Env -> Occ -> GVal
lookupVal env (GblOcc val)= val
lookupVal env (LclOcc id) = fromJust (Map.lookup id env)

lookupChan :: Env -> Occ -> Chan	-- Channel expected here
lookupChan env occ = unGChan (lookupVal env occ)

lookupLit :: Env -> Occ -> Double
lookupLit env occ = unGLit (lookupVal env occ)

mkEnv :: [LclId] -> [GVal] -> Env
mkEnv = extendEnv "mkEnv" emptyEnv

extendEnv :: String -> Env -> [LclId] -> [GVal] -> Env
extendEnv str env ids cs
  | length ids /= length cs = error ("extendEnv " ++ str ++ " " ++ show ids ++ show cs)
  | otherwise
  = foldl2 add env ids cs
  where
    add env id c = Map.insert id c env


-------------------------------------------------
--		Eq and Ord instances
-------------------------------------------------

-- Names
data LclId = LclId Unique String	-- Locally unique

instance Eq LclId where
   (LclId id1 _) == (LclId id2 _) = id1 == id2
instance Ord LclId where
   (LclId id1 _) `compare` (LclId id2 _) = id1 `compare` id2

instance Eq QAbs where
   q1 == q2 = qa_id q1 == qa_id q2
instance Ord QAbs where
   q1 `compare` q2 = qa_id q1 `compare` qa_id q2

instance Eq Macro where
   (Macro m1 _ _) == (Macro m2 _ _) = m1 == m2
instance Ord Macro where
   (Macro m1 _ _) `compare` (Macro m2 _ _) = m1 `compare` m2


-------------------------------------------------
--		Show instances
-------------------------------------------------

instance Show LclId where
  show (LclId u s) = s ++ "." ++ show u

instance Show (PlotItem proc chan) where
  show (PlotProc _ s)   = s
  show (PlotChan _ _ s) = s

instance Show GVal where
   show (GAbs f)   = show f
   show (GMacro m) = show m
   show (GChan c)  = show c
   show (GLit l)   = show l

showPops :: Soup -> IO ()
showPops soup 
  = hPutStrLn (sl_handle log) log_line
  where
    procs = soup_procs soup
    chans = soup_chans soup
    log   = soup_log soup

    log_line = show (soup_time soup) ++ ", " ++
  	 --    show (soup_steps soup) ++ ", " ++
	       showWithCommas (map show_item (sl_plot log))

    show_item :: PlotItem QAbs Chan -> Int
    show_item (PlotProc p _) = fromJust (Map.lookup p procs)
    show_item (PlotChan c mode _) 
	= procSetSize (mix_procs rs) + procSetSize in_or_out
	where
	  rs = fromJust (Map.lookup c chans)
	  in_or_out = case mode of
			InMode  -> in_procs rs
			OutMode -> out_procs rs

showHeaders :: Soup -> IO ()
showHeaders soup 
  = hPutStrLn (sl_handle log) header_line
  where
    log = soup_log soup
    header_line = "Time, " ++ showWithCommas (sl_plot log)

showDebug, showDebugHeaders :: Soup -> IO ()
-- Dump to stderr
showDebugHeaders soup
  | not (sl_debug (soup_log soup))
  = return ()
  | otherwise
  = hPutStrLn stderr (concat (intersperse ", " items))
  where
    items = [ "Time", "Steps", "Live proces",
	      "Active delays", "Procs waiting on delays",
	      "Active channels", "Procs waiting on channels" ]

showDebug soup
  | not (sl_debug (soup_log soup))
  = return ()
  | otherwise
  = hPutStrLn stderr (show (soup_time soup) ++ ", " ++ 
	              showWithCommas items)
  where
    items :: [Int]
    items = [ soup_steps soup
	    , length delay_prs, sum (map (readySetCount . snd) delay_prs)
	    , length chan_prs, sum (map (readySetCount . snd) chan_prs)
	    ]

    (delay_prs, chan_prs) = partition is_delay (Map.toList (soup_chans soup))
    is_delay (_,DelayRS {}) = True
    is_delay (_,ChanRS {})  = False

instance Show QProc where
   show (QProc fn args) = show fn ++ show args

instance Show QAbs where
   show fn = show (qa_id fn)

instance Show Macro where
   show (Macro m _ _) = show m

showWithCommas :: Show a => [a] -> String
showWithCommas [] = []
showWithCommas [x] = show x
showWithCommas (x:xs) = show x ++ ", " ++ showWithCommas xs


------------------------
foldl2 k z []     []     = z
foldl2 k z (x:xs) (y:ys) = foldl2 k (k z x y) xs ys 

