module Main where

import LLSTM2
import Control.Concurrent
import GHC.Conc
import Control.Monad
import System.Random 
--import System.Time
import Text.Printf
import System.Environment
--import TStats
--this benchmark will attempt to compare a two-threaded list with conflicts with one without conflicts.

foo :: (a, b, c, d, e) -> b
foo (_, b, _, _, _) = b


createThread :: Int -> TVar ListNode -> Int -> MVar Int -> IO ThreadId
createThread numOps tList maxNumber mvar = forkIO $ do
	callNTimes numOps $ do
		rnd1 <- randomRIO (1::Int, maxNumber)
		rnd2 <- randomRIO (1::Int, maxNumber)
		atomically $ deleteListNode tList rnd1
		atomically $ insertListNode tList rnd2
 	putMVar mvar 1

createThreads :: Int -> Int -> TVar ListNode -> Int -> [MVar Int] -> IO()
createThreads n numOps tList maxNumber mvars = mapM_ (createThread numOps tList maxNumber)	mvars
	

main1 :: Int -> Int -> Int -> IO () 
main1 numops listLength numThreads = do
	ourList <- atomically (createSampleList (reverse [x | x <- [1 .. listLength], ((mod x 2) == 0)]))
	ourListAsString <- atomically (toString ourList)
--	timeStart <- getClockTime
	ourTList <- newTVarIO ourList
	--; listAsString <- atomically (toString ourList)
	putStrLn (show (length ourListAsString))
	mvars <- replicateM numThreads newEmptyMVar
	print(numops)
	threads <- createThreads numThreads numops ourTList listLength mvars
	mapM_ takeMVar mvars
--	timeEnd <- getClockTime
	ourList2 <- atomically(readTVar ourTList)
	listAsString <- atomically (toString ourList2)
	putStrLn (show (length listAsString))
--	let diff = (normalizeTimeDiff (diffClockTimes timeEnd timeStart))
--	print ((((fromIntegral(tdPicosec diff))/(10^12))) + (fromIntegral((tdSec diff) + (60 * (tdMin diff)) + (3600 * (tdHour diff)))))
	return ()

callNTimes :: Int -> IO () -> IO  ()
callNTimes 0 _ = return ()
callNTimes times f = do
 	f
 	callNTimes (times - 1) f

main :: IO ()
main = do
 	args <- getArgs
 	let numops = read (args!!0)
 	let listlength = read (args!!1)
 	let numthreads = read (args!!2)
 	let numiterations = read (args!!3)
 	callNTimes numiterations $ main1 numops listlength numthreads
