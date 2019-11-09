module Main where

import Control.Concurrent
import GHC.Conc
import Control.Monad
--import Time
import Text.Printf
import System.Environment

createThread :: Int -> TVar Int -> MVar Int -> IO ThreadId
createThread numOps tValue mvar = forkIO $ do
	callNTimes numOps $ do
		atomically $ do
			theValue <- readTVar tValue
			writeTVar tValue (theValue + 1)
			return ()
	putMVar mvar 1


createThreads :: Int -> Int -> TVar Int -> [MVar Int] -> IO()
createThreads n numOps tVar mvars = mapM_ (createThread numOps tVar) mvars


main1 :: Int -> Int -> IO ()
main1 numops numThreads = do
	-- putStrLn ("Thread: " ++ show (numThreads) ++ " Ops: " ++ show(numops))
	theSharedInt <- newTVarIO 0
--	timeStart <- getClockTime
	mvars <- replicateM numThreads newEmptyMVar
	print(numops)
	threads <- createThreads numThreads numops theSharedInt mvars
	mapM_ takeMVar mvars
--	timeEnd <- getClockTime
	theSharedIntValue <- atomically $ readTVar theSharedInt
	putStrLn $ show theSharedIntValue
-- dumpSTMStats
--	let diff = normalizeTimeDiff $ diffClockTimes timeEnd timeStart
--	print ((((fromIntegral(tdPicosec diff))/(10^12))) + (fromIntegral((tdSec diff) + (60 * (tdMin diff)) + (3600 * (tdHour diff)))))
	return ()


callNTimes :: Int -> IO () -> IO ()
callNTimes 0 _ = return ()
callNTimes times f = do
	f
	callNTimes (times - 1) f


main :: IO()
main = do
	args <- getArgs
	let numops = read (args!!0)
	let numthreads = read (args!!1)
	let numiterations = read (args!!2)
	callNTimes numiterations (main1 numops numthreads)
