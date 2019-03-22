-- Basic concurrency abstractions

module Event where
-- BCP: Needs a proper module header, etc.

import Control.Concurrent.STM

newtype Event = E (TVar Bool)
newEvent :: STM Event
newEvent = do { v <- newTVar False; return (E v) }

signalEvent :: Event -> STM ()
signalEvent (E v) = writeTVar v True

awaitEvent :: Event -> STM ()
awaitEvent (E v) = do { b <- readTVar v
		      ; if b then return () else retry }

------------------------------------------------------------------------

data CEvent = CE (TVar Int) 	-- Remaining count
		 Event		-- Signalled when count falls to zero

newCEvent :: Int -> STM CEvent
newCEvent n = do { v <- newTVar n; e <- newEvent; return (CE v e) }

signalCEvent :: CEvent -> STM Bool
-- Returns True iff this signal made the value zero
signalCEvent (CE v e) = do { n <- readTVar v
		           ; writeTVar v (n-1)
			   ; if n==1 
			     then do { signalEvent e; return True }
			     else return False }

awaitCEvent :: CEvent -> STM ()
awaitCEvent (CE v e) = awaitEvent e

------------------------------------------------------------------------

data Barrier 
  = B { participants :: Int,
	state :: TVar CEvent }

mkBarrier :: Int -> STM Barrier
-- Make a new N-barrier
mkBarrier n = do { c <- newCEvent n
                 ; s <- newTVar c
                 ; return (B { participants=n, state = s })}

awaitBarrier :: Barrier -> IO ()
-- Wait until N proceess have called awaitBarrier,
-- let them all proceed, and reset the barrier
-- NB: this is an IO operation!  
awaitBarrier (B { participants = n, state = ce_var })
  = do	{ mb_q <- atomically (do 
		{ ce <- readTVar ce_var
	 	; we_are_last <- signalCEvent ce
		; if we_are_last then
			-- It's our job to re-initialise the barrier
			do { ce' <- newCEvent n
			   ; writeTVar ce_var ce'
			   ; return Nothing }
		      else 
			-- We are not the last to reach the barrier
			do { return (Just ce) }})

	-- The first 'atomic' returns (Just q) if we are *not* 
	-- the last, and hence must wait on q.
	-- We must do this in a separate transaction so that other
	-- threads can see our signalCEvent (above), and vice versa.
	-- Otherwise the awaitCEvent would wait for ever!
	; case mb_q of
 		Nothing -> return ()
	        Just q  -> atomically (awaitCEvent q)
	}


----------------------------------------------------------------------
-- An earlier version of barriers...

{-

data Barrier 
  = B { max :: Int,
	state :: TVar (Int, Event) }

mkBarrier :: Int -> STM Barrier
-- Make a new N-barrier

awaitBarrier :: Barrier -> IO ()
-- Wait until N proceess have called awaitBarrier,
-- let them all proceed, and reset the barrier
awaitBarrier (B { max = n, state = var })
  = do { mb_q <- atomic (do { (r,q) <- readTVar var
		    ; if r==1 then
			-- We're the last to reach the barrier
			-- so it's our job to re-initialise the barrier
			-- and tell everyone else to go
			do { signalEvent q	-- Tell others to go
			   ; q' <- newEvent
			   ; writeTVar var (n,q')
			   ; return Nothing }
		      else 
			-- We are not the last to reach the barrier
			do { writeTVar var (n-1,q)
			   ; return (Just q) }})

	-- The first 'atomic' returns (Just q) if we are *not* 
	-- the last, and hence must wait on q.
	; case mb_q of
		Nothing -> return ()
		Just q  -> atomic { awaitEvent q }
	}

-}