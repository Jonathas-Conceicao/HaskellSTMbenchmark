 module Main where

 import BinaryTree
 import Control.Concurrent
 import GHC.Conc
 import Control.Monad
 import Random 
 import System.Time
 import Text.Printf
 import System

--this benchmark will attempt to compare a two-threaded Tree with conflicts with one without conflicts.

 createThread :: Int -> TVar Tree -> Int -> MVar Int -> IO ThreadId
 createThread numOps tTree maxNumber mvar =
 	 forkIO ( do
			{ callNTimes numOps 
				(do 
				{ rnd1 <- randomRIO (1::Int, 10)
				; rnd2 <- randomRIO (1::Int, maxNumber)
				; case rnd1 of
				; 1 -> do {atomically (deleteTree tTree rnd2)
									; return ()}
				; 2 -> do {atomically (insertTree tTree rnd2)
									; return ()}
				; otherwise -> do {atomically (lookupTree tTree rnd2)
									; return ()}
				}) 
				; putMVar mvar 1
			})

 createThreads :: Int -> Int -> TVar Tree -> Int -> [MVar Int] -> IO()
 createThreads n numOps tTree maxNumber mvars
	= mapM_ 
		(createThread numOps tTree maxNumber)
		mvars

 callNTimesDeterministic 0 _ _ _ = return()
 callNTimesDeterministic numOps maxNumber tTree discriminant =
				do 
				{ let rnd1 = (numOps `div` 5) + (100000 * discriminant)
				; let rnd2 = (numOps `div` 3) + (100000 * discriminant)
				--; putStrLn ("Deleting " ++ (show rnd1));
				; atomically (deleteTree tTree rnd1)
				--; putStrLn ("Inserting " ++ (show rnd2));
				; atomically (insertTree tTree rnd2)
				; callNTimesDeterministic (numOps - 1) maxNumber tTree discriminant
			      	}

 deterministicThread :: Int -> TVar Tree -> Int -> MVar Int -> Int -> IO ThreadId
 deterministicThread numOps tTree maxNumber mvar discriminant=
 	 forkIO ( do
			{ callNTimesDeterministic numOps maxNumber tTree discriminant
			; putMVar mvar 1
			}
          	 )

 deterministicThreads :: Int -> Int -> TVar Tree -> Int -> [MVar Int] -> IO()
 deterministicThreads n numOps tTree maxNumber mvars
	= do {
		deterministicThread numOps tTree maxNumber (mvars!!0) 0
		; deterministicThread numOps tTree maxNumber (mvars!!1) 1
		; return ()
	}


 main1 :: Int -> Int -> Int -> IO () 
 main1 numops treeSize numThreads = do
	{ ourTTree <- newTVarIO Nil 
	; atomically (createSampleTree [x | x <- [1 .. treeSize], ((x `mod` 2) == 0)] ourTTree)
	; ourTree <- atomically (readTVar ourTTree)
	; --ourTreeAsString <- atomically (toString ourTree)

	; timeStart <- getClockTime
	; ourTTree <- newTVarIO ourTree

	--; TreeAsString <- atomically (toString ourTree)
	; --putStrLn (show (length ourTreeAsString))

	; mvars <- replicateM numThreads newEmptyMVar
	; print(numops)

	; threads <- createThreads numThreads numops ourTTree treeSize mvars

	; mapM_ takeMVar mvars
	

	; timeEnd <- getClockTime
	; --ourTree2 <- atomically(readTVar ourTTree)
	; --treeAsString <- atomically (toString ourTree2)
	; --putStrLn (show (length treeAsString))

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
		; let treesize = read (args!!1)
		; let numthreads = read (args!!2)
		; let numiterations = read (args!!3)
		; callNTimes numiterations (main1 numops treesize numthreads)
	      }


