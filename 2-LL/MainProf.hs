 module Main where

 import LLSTM2
 import Control.Concurrent
 import GHC.Conc
 import Control.Monad
 import System.Random as Random
 import System.Time
 import Text.Printf
 import System

--this benchmark will attempt to compare a two-threaded list with conflicts with one without conflicts.

 boolTo :: STM Bool -> STM ()
 boolTo something = return ()

 createThread :: Int -> TVar ListNode -> Int -> MVar Int -> IO ThreadId
 createThread numOps tList maxNumber mvar =
 	 forkIO ( do
			{ callNTimes numOps 
				(do 
				{ rnd1 <- randomRIO (1::Int, 10)
				; rnd2 <- randomRIO (1::Int, maxNumber)
				; case rnd1 of
				; 1 -> do {atomicallyProfiled (deleteListNode tList rnd2) 1#
									; return ()}
				; 2 -> do {atomicallyProfiled (insertListNode tList rnd2) 2#
									; return ()}
				; otherwise -> do {atomicallyProfiled (lookupListNode tList rnd2) 3#
									; return ()}
				}) 
				; putMVar mvar 1
			})

 createThreads :: Int -> Int -> TVar ListNode -> Int -> [MVar Int] -> IO()
 createThreads n numOps tList maxNumber mvars
	= mapM_ 
		(createThread numOps tList maxNumber)
		mvars


 main1 :: Int -> Int -> Int -> IO () 
 main1 numops listLength numThreads = do
	{ ourList <- atomicallyProfiled (createSampleList (reverse [x | x <- [1 .. listLength], ((mod x 2) == 0)])) 4#
	; --ourListAsString <- atomically (toString ourList)
	--; listAsString <- atomically (toString ourList)
	; --putStrLn (show (length ourListAsString))

	; timeStart <- getClockTime
	; ourTList <- atomicallyProfiled (newTVar ourList) 5#

	; mvars <- replicateM numThreads newEmptyMVar
	; print(numops)

	; threads <- createThreads numThreads numops ourTList listLength mvars

	; mapM_ takeMVar mvars
	
	; timeEnd <- getClockTime
	; ourList2 <- atomicallyProfiled (readTVar ourTList) 6#
	; listAsString <- atomicallyProfiled (toString ourList2) 7#
	; putStrLn (show (length listAsString))

	; let diff = (normalizeTimeDiff (diffClockTimes timeEnd timeStart))
	
	; stats <- readTStats
	; putStrLn (show stats)
		
	; print ((((fromIntegral(tdPicosec diff))/(10^12))) + (fromIntegral((tdSec diff) + (60 * (tdMin diff)) + (3600 * (tdHour diff)))))

	; return ()
	}

 callNTimes :: Int -> IO () -> IO ()
 callNTimes 0 _ = return ()
 callNTimes times f = do{f 
			;callNTimes (times-1) f
			-- ;return ()
			}

 callNTimesSTM :: Int -> STM () -> STM ()
 callNTimesSTM 0 _ = return ()
 callNTimesSTM times f = do{f 
			;callNTimesSTM (times-1) f
			--;return ()
			}

 main :: IO()
 main =    do { args <- getArgs
		; let numops = read (args!!0)
		; let listlength = read (args!!1)
		; let numthreads = read (args!!2)
		; let numiterations = read (args!!3)
		; callNTimes numiterations (main1 numops listlength numthreads)
	      }


