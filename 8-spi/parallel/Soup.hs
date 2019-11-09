{-# OPTIONS -fglasgow-exts #-}

module Soup( 
	Soup(soup_id, soup_time, soup_deadline, soup_steps,
		soup_uniqs, soup_rng, soup_ncells), 
		-- But not soup_chans, soup_procs, which 
		-- have mutual invariants

	PlotItem(..), PlotSpec,
	Guard(..), Proc(..), 
	QAbs(..), QProc(..), QProcs, Mode(..), Gate(..),
	GblId, LclId(..), Unique,
	Macro(..), NewChanBndr,
	ChanOcc, Occ(..), GVal(..), 
	Weight, Time,

	ProcSet, procSetSize, procSetItems, delFromProcSet, 
	ReadySets(..), readySetActivity,

	emptySoup, extendSoup, 
	delFromSoup, delProcsFromSoup,
	bumpTime, bumpDeadline,
	readySets, soupReadySets, soupProcs, soupProcSet,
	showDebugHeaders, showDebug, 

	Env, emptyEnv, mkEnv, extendEnv, 
	lookupChan, lookupLit, lookupVal, lookupVals, 

	countPops,	-- Logging 

        debugOn,
	
        -- this belongs someplace else (a Util module?)
        showWithCommas

  ) where

import Chan( Chan, mkDelayChan, Rate, GblId, Unique ) 
import qualified Data.Map as Map
import Data.Maybe	( fromJust )
import System.IO	( Handle, hPutStrLn, stderr )
import Data.List	( intersperse, partition )
import qualified System.Random as Random

debugOn = False

-------------------------------------
--	The Soup type and friends
-------------------------------------

data Soup = Soup { 
        soup_id       :: Int,   -- Replicated from cl_id
	soup_ncells   :: Int,	-- ...cl_ncells

	soup_time     :: Time,	-- Current time, starts at zero
	soup_deadline :: Time, 	-- Next deadline
	soup_steps :: Int,	-- Number of steps (for entertainment)

	soup_uniqs :: Unique,		-- Next available unique
	soup_rng   :: Random.StdGen,	-- Random number supply

	soup_procs :: ProcSet,
		-- The active QProcs in the soup

	soup_chans :: Map.Map Chan ReadySets
		-- Contains the same inforamtion as soup_procs, but
		-- organised for fast access during simulation
		-- Each active QProc may occur many times, once for
		-- each channel it is ready to fire on
		--
		-- The connection between soup_procs and soup_chans is
		-- maintained entirely inside this module
  }			

soupProcSet :: Soup -> ProcSet
soupProcSet = soup_procs

soupProcs :: Soup -> [QProcs]
soupProcs soup = case soup_procs soup of
		   ProcSet _ proc_map -> Map.toList proc_map

soupReadySets :: Soup -> [(Chan, ReadySets)]
soupReadySets soup = Map.toList (soup_chans soup)


-- BCP: Don't understand why this has to be parameterized, instead of just
-- referring to types QAbs and Chan directly
data PlotItem proc chan 
  = PlotProc proc String | PlotChan chan Mode String

type PlotSpec = [PlotItem QAbs Chan]

-- Activity = sum of all the rates of all the processes
-- [BCP: Where does this comment belong?]

emptySoup :: Int -> Int -> Unique -> Random.StdGen -> Soup
emptySoup id ncells uniq rng
  = Soup { soup_id = id, soup_ncells = ncells,
	   soup_uniqs = uniq,
	   soup_rng = rng,
           soup_time = 0, 
           soup_steps = 0, 
           soup_deadline = 0,
	   soup_chans = Map.empty, 
	   soup_procs = emptyProcSet
         }

bumpDeadline :: Time -> Soup -> Soup
bumpDeadline delta_t soup = 
  soup { soup_deadline = soup_deadline soup + delta_t }

bumpTime :: Time -> Soup -> Soup
bumpTime delta_t soup = soup { soup_time = soup_time soup + delta_t,
			       soup_steps = soup_steps soup + 1 }

trackAllProcs = False

extendSoup :: [QProcs] -> Soup -> Soup
-- Add n times the procs to the soup
extendSoup ps soup 
  = foldr add soup ps
  where
    add :: QProcs -> Soup -> Soup
    add p soup = foldr (add_gate p) (add_to_procs p soup) (gates p)

    add_to_procs p soup@(Soup { soup_procs = procs })
 	= soup { soup_procs = extendProcSet p procs }

    add_gate :: QProcs -> Gate Chan -> Soup -> Soup
    add_gate p (Gate mode chan) soup@(Soup { soup_chans = chans })
	= soup { soup_chans = Map.adjust (updateRS mode (extendProcSet p)) 
					 chan chans' }
	where
	  chans' | chan `Map.member` chans = chans
		 | otherwise = Map.insert chan (emptyRS mode) chans

delProcsFromSoup :: [QProc] -> Soup -> Soup
-- Delete one of each of these
delProcsFromSoup ps soup = foldr (\p s -> delFromSoup (p,1) s) soup ps

delFromSoup :: QProcs -> Soup -> Soup
delFromSoup p soup = foldr (del_gate p) (del_from_procs p soup) (gates p)
  where
    del_from_procs p soup@(Soup { soup_procs = procs })
	= soup { soup_procs = delFromProcSet p procs }

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
updateRS mode upd rs@(ChanRS {})  = 
                              case mode of
                                InMode  -> rs { in_procs  = upd (in_procs rs) }
                                OutMode -> rs { out_procs = upd (out_procs rs) }
                                MixMode -> rs { mix_procs = upd (mix_procs rs) }

readySetActivity :: ReadySets -> Int	-- Number of enabled interactions
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

procSetItems :: ProcSet -> [QProcs]
procSetItems (ProcSet _ ps) = Map.toList ps

delFromProcSet :: QProcs -> ProcSet -> ProcSet
-- Precondition: the proc is currently in the set
delFromProcSet (p,k) (ProcSet n mp) 
  = ProcSet (n-k) (Map.updateWithKey upd p mp)
  where
    upd p n | n == k    = Nothing
	    | n < k     = error "delFromProcSet"
	    | otherwise = Just (n-k)

extendProcSet :: QProcs -> ProcSet -> ProcSet
extendProcSet (p,n) (ProcSet k mp)
  = ProcSet (k+n) (Map.insertWith (+) p n mp)



--------------------------------------
-- Logging

countPops :: Soup -> PlotSpec -> [Int]
-- Return the counts in 1-1 correspondance with the PlotSpec items
countPops soup plot_spec
  = map count_item plot_spec
  where
    ProcSet _ proc_map = soup_procs soup
    proc_count_prs = Map.toList proc_map
    chans = soup_chans soup

    count_item :: PlotItem QAbs Chan -> Int
    count_item (PlotProc p _) 
	= sum [n | (QProc q _, n) <- proc_count_prs, p==q]
    count_item (PlotChan c mode _) 
	| Just rs <- Map.lookup c chans
	= let in_or_out = case mode of
				InMode  -> in_procs rs
				OutMode -> out_procs rs
	  in procSetSize (mix_procs rs) + procSetSize in_or_out

	| otherwise
 	= 0	-- No processes in the soup are waiting on c

--------------------------------------
-- Channels
type Time = Double
type Activity = Double
type Weight = Double	-- Rate * Activity

-- Processes
-- Question: can a single choice have two identical guards?
-- [BCP: Yes, it certainly can.]

type QProcs = (QProc, Int)	-- N copies of this QProc

data QProc 	-- A process application
		-- These are dynamically generated
  = QProc QAbs [GVal]
  deriving( Eq, Ord )

procAbs :: QProc -> QAbs
procAbs (QProc fn _) = fn

data QAbs	-- A process abstraction
		-- There are statically-fixed number of these
  = QAbs {
	qa_id     :: GblId,		-- Id of the abstraction
	qa_params :: [LclId], 		-- Parameters
	qa_new    :: [NewChanBndr],	-- New channels
	qa_alts   :: [(Guard, Proc)],	-- Choices
	qa_gates  :: [Gate Occ]		-- Cached: the gates for this abstraction
					-- 	Avoids recomputation of InOutMode
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

gates :: QProcs -> [Gate Chan]
gates (QProc fn args, _)
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

showDebugHeaders :: IO ()
-- Dump to stderr
showDebugHeaders 
  | not debugOn
  = return ()
  | otherwise
  = hPutStrLn stderr (concat (intersperse ", " items))
  where
    items = [ "Time", "Id", "Steps", "Live proces",
	      "Active delays", "Procs waiting on delays",
	      "Active channels", "Procs waiting on channels" ]

showDebug :: Soup -> IO ()
showDebug soup
  | not debugOn
  = return ()
  | otherwise
  = hPutStrLn stderr (show (soup_time soup) ++ ", " ++ 
	              showWithCommas items)
  where
    items :: [Int]
    items = [ soup_id soup, soup_steps soup
	    , length delay_prs, sum (map (readySetCount . snd) delay_prs)
	    , length chan_prs, sum (map (readySetCount . snd) chan_prs)
	    ]

    (delay_prs, chan_prs) = partition is_delay (Map.toList (soup_chans soup))
    is_delay (_,DelayRS {}) = True
    is_delay (_,ChanRS {})  = False

instance Show Soup where
   show soup = "Soup " ++ show (soup_id soup) ++ " {" ++ concat (map show_item (soupProcs soup)) ++ "}"
    where
      show_item (p,n) = " " ++ show n ++ "*(" ++ show p ++ ") "

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

