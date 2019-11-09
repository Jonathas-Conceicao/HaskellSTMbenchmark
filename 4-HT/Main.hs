module Main where

import Data.THash
import Control.Concurrent
import GHC.Conc
import Control.Monad
 -- import Random
import System.Random 
import System.Time
import Text.Printf
import System.Environment

{-
 main :: IO()
 main =    do { 
		
		; atomically (
			do
			{ 
			; insert ourHashTable 25 "Nehir"
			; insert ourHashTable 24 "Cristian"
			; insert ourHashTable 31 "Paul"
			; insert ourHashTable 18 "Mariana"
			; delete ourHashTable 25 })
		; ourValues <- atomically (values ourHashTable)
		; allPairs <- atomically (each ourHashTable)
		; resultLookup <- atomically (Data.THash.lookup ourHashTable 24)
		; print ourValues
		; print allPairs
		; print resultLookup
		; print (hashInt 24)
	}
-}

createThread :: Int -> THash Int String -> Int -> MVar Int -> IO ThreadId
createThread numOps ourHashTable maxNumber mvar = forkIO $ do
	callNTimes numOps $ do
		rnd1 <- randomRIO (1::Int, 10)
		rnd2 <- randomRIO (1::Int, maxNumber)
		case rnd1 of
			1 -> do
				atomically (delete ourHashTable rnd2)
				return ()
			2 -> do
				atomically (insert ourHashTable rnd2 "Cristian")
				return ()
			otherwise -> do
				atomically (Data.THash.lookup ourHashTable rnd2)
				return ()
	putMVar mvar 1


createThreads :: Int -> Int -> THash Int String -> Int -> [MVar Int] -> IO()
createThreads n numOps tHash maxNumber mvars = mapM_ (createThread numOps tHash maxNumber)	mvars

	
main1 :: Int -> Int -> Int -> IO ()
main1 numops keysRange numThreads = do
 	ourHashTable <- atomically (new hashInt)
	--ourHash <- atomically (createSampleHash (reverse [x | x <- [1 .. keysRange], ((mod x 2) == 0)]))
	timeStart <- getClockTime
	mvars <- replicateM numThreads newEmptyMVar
	print(numops)
	threads <- createThreads numThreads numops ourHashTable keysRange mvars
	mapM_ takeMVar mvars
	timeEnd <- getClockTime
	allPairs <- atomically (each ourHashTable)
	print allPairs
	let diff = (normalizeTimeDiff (diffClockTimes timeEnd timeStart))
	--; stats <- readTStats
	--; putStrLn (show stats)
	print ((((fromIntegral(tdPicosec diff))/(10^12))) + (fromIntegral((tdSec diff) + (60 * (tdMin diff)) + (3600 * (tdHour diff)))))
	return ()


callNTimes :: Int -> IO () -> IO ()
callNTimes 0 _ = return ()
callNTimes times f = do
 	f 
	callNTimes (times-1) f
	

main :: IO()
main = do
 	args <- getArgs
	let numops = read (args!!0)
	let keysrange = read (args!!1)
	let numthreads = read (args!!2)
	let numiterations = read (args!!3)
	callNTimes numiterations (main1 numops keysrange numthreads)