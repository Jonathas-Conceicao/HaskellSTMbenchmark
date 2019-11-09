module Cell where

import Chan
import Event
import Soup    
import RunSoup

--import IO
import System.IO	--( fixIO )
import Control.Concurrent.STM
import Control.Concurrent ( forkIO )
import Control.Monad	( foldM )
import Data.Maybe	( fromJust )
import qualified Data.Map as Map
import qualified System.Random as Random

data Cell = Cell { 
	cl_soup  :: TVar Soup,	    -- This cell's soup (modified by others)
	cl_finish :: Time,	    -- Overall finish time for simulation
        cl_done :: CEvent,          -- CEvent to signal before terminating
	cl_ncells :: Int,	    -- Total number of cells
	cl_epochLength :: Time,	    -- Interval between exchange of processes
        cl_id :: Int,               -- Which cell am I?  1-indexed.
	cl_export :: Float,	    -- What %age to export in each epoch (0..1)
	cl_barrier :: Barrier,      -- Shared barrier, for syncing at end of epoch
        cl_neighbours :: [Cell],    -- "Adjacent" cells
	cl_log :: Log,		    -- Global, shared, mutable log-state
	cl_handle :: Handle         -- File handle for logging
    }

debug cell s
  | debugOn   = putStrLn ("[" ++ (show (cl_id cell)) ++ "] " ++ s)
  | otherwise = return ()

-- BCP: Do the uses of atomically here seem right?  I guess so.
runCells log_handle (finish,max_logs) plots init_procs 
	 nCells epochLength export_fraction
  = do	{ ce <- atomically (newCEvent nCells)
        ; log <- atomically (newLog nCells plots max_logs log_handle)
        ; showHeaders log
	; cells <- makeCells log_handle finish nCells epochLength export_fraction log ce
	; let go c = if (cl_id c) == 1 then 
			-- Inject the initial processes into one soup only!
                       do { addInitProcs c init_procs; 
                          ; runCell c }
                     else runCell c
              f c = forkIO (go c)
        ; mapM f cells   	          -- Spawn a thread for each cell
	; atomically (awaitCEvent ce)     -- Wait for everyone to finish
	}

makeCells :: Handle 
	  -> Time 	-- Finish time
	  -> Int	-- Numbers of cells
	  -> Time	-- Length of one epoch
	  -> Float	-- Export fraction
	  -> Log	-- Global log state
	  -> CEvent	-- Signal this when terminating
	  -> IO [Cell]
makeCells log_handle finish nCells epochLength export_fraction log ce
  = do { barrier <- atomically (mkBarrier nCells)
       ; let makeCell all_cells my_id = 
               do { rng <- Random.newStdGen
		  ; let soup = emptySoup my_id nCells (firstUnique my_id) rng
		  ; soup_var <- atomically (newTVar soup)

                  ; return (Cell { 
                      cl_soup  = soup_var,
                      cl_finish = finish,
                      cl_done = ce,
		      cl_export = export_fraction,
                      cl_ncells = nCells,
                      cl_epochLength = epochLength,
                      cl_id = my_id,
                      cl_barrier = barrier,

                      cl_neighbours = filter (\c -> cl_id c /= my_id) all_cells,
			-- Make all cells (other than me) into my neighbours
			-- Later, we might choose a smaller set of neighbors.

                      cl_log = log,
                      cl_handle = log_handle
                    })}
       ; fixIO ( \ cells -> mapM (makeCell cells) [1..nCells]) }

addInitProcs cell init_procs = updateField_ cl_soup cell $
			       initSoup init_procs

-- Here's a version of runCell that "works" enough to go on with for the 
-- moment, but may not be as nice as possible... see Notes file
runCell :: Cell -> IO ()
runCell cell
  = do	{ debug cell "starting"

        -- Run until the next deadline
        ; updateField_ cl_soup cell (runSoup (cl_epochLength cell))

        ; doLogging cell

	; exports <- computeExports cell

        -- Wait for everybody else to catch up, if necessary
        -- (Note that we don't want to let anybody start exporting processes
        -- to other cells until everybody has finished runSoup, because
        -- this would almost certainly step on the above transaction.)
        ; debug cell ("at barrier 1")
	; awaitBarrier (cl_barrier cell)

        -- Export the previously-computed exports
	; exportProcesses cell exports

        -- Wait for everybody get here before starting over
        ; debug cell ("at barrier 2")
	; awaitBarrier (cl_barrier cell)

	-- Either loop, or tell main process that we are done
        ; done <- simulationFinished cell
	; if not done then runCell cell 
          else do { debug cell ("terminating")
                  ; atomically (signalCEvent (cl_done cell)); 
                  ; return () }}

simulationFinished cell
  = do { soup <- readField cl_soup cell
       ; return (soup_deadline soup > cl_finish cell) }

------------------------------------------------------------------------------
-- Exporting

computeExports :: Cell -> IO [Exports]
-- Export a fixed percentage of the processes in the cell to its neighbours
computeExports cell
  = updateField cl_soup cell $
    extractExports (length (cl_neighbours cell)) (cl_export cell)

exportProcesses :: Cell -> [Exports] -> IO ()
exportProcesses cell exports
  = mapM_ export_to (cl_neighbours cell `zip` exports)
  where
    export_to (cell, exports) = updateField_ cl_soup cell $
				extendSoup exports


------------------------------------------------------------------------------
-- Logging

data Log = Log {
	log_max   :: Int,	     -- Number of desired log lines (constant)
	log_lines :: Int,	     -- Number of lines so far (starts zero)
        log_handle :: Handle,        -- Where to print to
	log_plotSpec :: PlotSpec,    -- What to plot (never changes)
	log_state :: TVar LogState   -- Counts for current epoch
	}	

type LogState = (Int, [Int])	    -- Number of log records awaited, plus
				    -- counts so far, in 1-1 with log_plotSpec

doLogging :: Cell -> IO ()
doLogging cell
  = do	{ log_due <- atomically (logDue cell)
	; if not log_due then return () else do
	{ mb_counts <- atomically (updateLogState cell)
        -- If we get back Just(counts), that means that we are the final
        -- thread to finish logging, so we should also print out the counts
	; case mb_counts of
	    Nothing     -> return ()
	    Just counts -> do { soup <- readField cl_soup cell
                              ; let time = soup_deadline soup
                              ; showCounts time counts (cl_log cell) }}}

newLog :: Int -> PlotSpec -> Int -> Handle -> STM Log
newLog nCells plot_spec max handle
  = do { state <- newTVar (initCounts nCells plot_spec)
       ; return (Log { log_max = max,
                       log_lines = 0,
                       log_handle = handle,
                       log_plotSpec = plot_spec,
                       log_state = state })}

logDue :: Cell -> STM Bool
logDue cell 
  = do 	{ soup <- readTVar (cl_soup cell)
	; let log = cl_log cell
	      next_log_time = 
                  (fromIntegral (log_lines log) / fromIntegral (log_max log)) 
                * cl_finish cell
	; return (next_log_time <= soup_time soup) }

updateLogState :: Cell -> STM (Maybe [Int])
updateLogState cell
  = do 	{ log_due <- logDue cell
	; if not log_due then return Nothing else do
		-- OK, so it's time to do logging for this cell
		-- First, compute the counts for this cell
	{ soup <- readTVar (cl_soup cell)
	; let log = cl_log cell
	      log_var = log_state log
	      cell_counts = countPops soup (log_plotSpec log)

		-- Now combine those counts into the global log
	; (n, counts_so_far) <- readTVar log_var
	; let counts_so_far' = zipWith(+) counts_so_far cell_counts

	; if n==1 then do    -- We are last: return aggregrate and reinitialise
		{ writeTVar log_var (initCounts (cl_ncells cell) (log_plotSpec log))
	  	; return (Just counts_so_far') }
	  else do	-- We are not the last: just update state
		{ writeTVar log_var (n-1,counts_so_far')
		; return Nothing }}}

initCounts :: Int -> PlotSpec -> LogState
initCounts n_cells plot_spec 
  = (n_cells, replicate (length plot_spec) 0)

showCounts :: Time -> [Int] -> Log -> IO ()
showCounts time counts log
  = hPutStrLn (log_handle log) log_line
  where
    log_line = show time ++ ", " ++ showWithCommas counts


showHeaders :: Log -> IO ()
showHeaders log
  = hPutStrLn (log_handle log) header_line
  where
    header_line = "Time, " ++ showWithCommas (log_plotSpec log)


------------------
readField :: (a -> TVar b) -> a -> IO b
readField sel record = atomically (readTVar (sel record))

writeField :: (a -> TVar b) -> a -> b -> IO ()
writeField sel record val = atomically (writeTVar (sel record) val)

updateField :: (a -> TVar b) -> a -> (b -> (b,r)) -> IO r
-- Atomically update the field
updateField sel record upd
 = atomically (do { v <- readTVar (sel record)
		  ; let (v',r) = upd v
		  ; writeTVar (sel record) v'
		  ; return r })

updateField_ :: (a -> TVar b) -> a -> (b -> b) -> IO ()
-- Atomically update the field
updateField_ sel record upd
 = atomically (do { v <- readTVar (sel record)
		  ; writeTVar (sel record) (upd v) })


