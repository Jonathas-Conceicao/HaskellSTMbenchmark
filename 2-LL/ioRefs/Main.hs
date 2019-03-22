 module Main where

 import LLIOREFS
-- import Control.Concurrent
-- import GHC.Conc
 import Control.Monad
 import Random 
 import System.Time
 import Text.Printf
 import System
 import GHC.IOBase

--this benchmark will attempt to compare a two-threaded list with conflicts with one without conflicts.

 executeOperations :: Int -> IORef ListNode -> Int -> IO ()
 executeOperations numOps tList maxNumber =
 	 do
			{ callNTimes numOps 
				(do 
				{ rnd1 <- randomRIO (1::Int, maxNumber)
				; rnd2 <- randomRIO (1::Int, maxNumber)
				--; putStrLn ("Deleting " ++ (show rnd1));
				; deleteListNode tList rnd1
				--; putStrLn ("Inserting " ++ (show rnd2));
				; insertListNode tList rnd2
			      	}) 
			}
          	 


 main1 :: Int -> Int -> IO () 
 main1 numops listLength = do
	{ ourList <- createSampleList (reverse [x | x <- [1 .. listLength], ((mod x 2) == 0)])
	; ourListAsString <- toString ourList

	; timeStart <- getClockTime
	; ourTList <- newIORef ourList

	--; listAsString <- atomically (toString ourList)
	; putStrLn (show (length ourListAsString))

	; print(numops)

	; executeOperations numops ourTList listLength

	; timeEnd <- getClockTime
	; ourList2 <- readIORef ourTList
	; listAsString <- toString ourList2
	; putStrLn (show (length listAsString))

	; let diff = (normalizeTimeDiff (diffClockTimes timeEnd timeStart))
	
	-- ;  stats <- readTStats
	-- ;  putStrLn (show stats)
		
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
		; let listlength = read (args!!1)
		; let numiterations = read (args!!2)
		; callNTimes numiterations (main1 numops listlength)
	      }


