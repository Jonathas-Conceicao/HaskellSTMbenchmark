module Main where

import Control.Concurrent
import GHC.Conc
import Control.Monad
import System.Time
import Text.Printf
import System.Environment
import Control.Monad.IO.Class (MonadIO(..))
import GHC.IO (IO(..))
import Data.IORef
import Data.Atomics
import GHC.Conc.Sync

instance MonadIO IOSTM where
    liftIO (IO a) = IOSTM a

atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
atomCAS ptr old new =
   atomicModifyIORefCAS ptr (\ cur -> if cur == old
                                   then (new, True)
                                   else (cur, False))


getID ::  IORef Int -> IO Int
getID idger = do
  v <- readIORef idger
  ok <- atomCAS idger v (v+1)
  if ok then return (v+1) else getID idger


createThread :: Int -> IORef Int -> MVar Int -> IO ThreadId
createThread numOps ioValue mvar = forkIO $ do
	callNTimes numOps $ do
		atomically $ performIO $ do
                  liftIO $ getID $ ioValue
                  return ()
	putMVar mvar 1

createThreads :: Int -> Int -> IORef Int -> [MVar Int] -> IO()
createThreads n numOps ioVar mvars = mapM_ (createThread numOps ioVar) mvars


main1 :: Int -> Int -> IO ()
main1 numops numThreads = do
	-- putStrLn ("Thread: " ++ show (numThreads) ++ " Ops: " ++ show(numops))
	theSharedInt <- newIORef 0
	timeStart <- getClockTime
	mvars <- replicateM numThreads newEmptyMVar
	-- print(numops)
	threads <- createThreads numThreads numops theSharedInt mvars
	mapM_ takeMVar mvars
	timeEnd <- getClockTime
	theSharedIntValue <- readIORef theSharedInt
	putStrLn $ show theSharedIntValue
-- dumpSTMStats
	let diff = normalizeTimeDiff $ diffClockTimes timeEnd timeStart
	print ((((fromIntegral(tdPicosec diff))/(10^12))) + (fromIntegral((tdSec diff) + (60 * (tdMin diff)) + (3600 * (tdHour diff)))))
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
