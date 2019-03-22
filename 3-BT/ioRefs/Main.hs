 module Main where

 import BinaryTree
 import Control.Concurrent
 import GHC.IOBase
 import Control.Monad
 import Random 
 import System.Time
 import Text.Printf
 import System

--this benchmark will attempt to compare a two-threaded Tree with conflicts with one without conflicts.

 executeOperations :: Int -> IORef Tree -> Int -> IO ()
 executeOperations numOps tTree maxNumber =
			do
			{ callNTimes numOps 
				(do 
				{ rnd1 <- randomRIO (1::Int, maxNumber)
				; rnd2 <- randomRIO (1::Int, maxNumber)
				--; putStrLn ("Deleting " ++ (show rnd1));
				; deleteTree tTree rnd1
				--; putStrLn ("Inserting " ++ (show rnd2));
				; insertTree tTree rnd2
			      	}) 
			}

{-
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

-}

 main1 :: Int -> Int -> Int -> IO () 
 main1 numops treeSize numThreads = do
	{ ourTTree <- newIORef Nil 
	; createSampleTree [x | x <- [1 .. treeSize], ((x `mod` 2) == 0)] ourTTree
	; ourTree <- readIORef ourTTree
	; ourTreeAsString <- toString ourTree

	; timeStart <- getClockTime
	; ourTTree <- newIORef ourTree

	--; TreeAsString <- atomically (toString ourTree)
	; putStrLn (show (length ourTreeAsString))


	; print(numops)
	
        ; executeOperations numops ourTTree treeSize


	

	; timeEnd <- getClockTime
	; ourTree2 <- readIORef ourTTree
	; treeAsString <- toString ourTree2
	; putStrLn (show (length treeAsString))

	; let diff = (normalizeTimeDiff (diffClockTimes timeEnd timeStart))
	
	-- ; stats <- readTStats
	-- ; putStrLn (show stats)
		
	; print ((((fromIntegral(tdPicosec diff))/(10^12))) + (fromIntegral((tdSec diff) + (60 * (tdMin diff)) + (3600 * (tdHour diff)))))

	; return ()
	}

 callNTimes :: Int -> IO () -> IO ()
 callNTimes 0 _ = return ()
 callNTimes times f = do{f 
			;callNTimes (times-1) f
			-- ;return ()
			}
{-
 callNTimesSTM :: Int -> STM () -> STM ()
 callNTimesSTM 0 _ = return ()
 callNTimesSTM times f = do{f 
			;callNTimesSTM (times-1) f
			--;return ()
			}
-}
 main :: IO()
 main =    do { args <- getArgs
		; let numops = read (args!!0)
		; let treesize = read (args!!1)
		; let numthreads = read (args!!2)
		; let numiterations = read (args!!3)
		; callNTimes numiterations (main1 numops treesize numthreads)
	      }


