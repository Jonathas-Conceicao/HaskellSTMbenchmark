module TBList where

import CASList
import AbstractLock
import GHC.Conc.Sync
import Control.Monad.IO.Class (MonadIO(..))
import GHC.IO (IO(..))

instance MonadIO IOSTM where
    liftIO (IO a) = IOSTM a

data ListHandle a = TBL ALock (CASList.ListHandle a)

newList :: Int -> IO (TBList.ListHandle Int)
newList size = do
  alock <- newALock size
  list <- CASList.newList
  return (TBL alock list)

add :: TBList.ListHandle Int -> Int -> IO Bool
add (TBL alock list) key = atomically $ performIO $ do
  ok <- liftIO $ lock alock key
  if ok
    then do
      addCommitHandler unlock
      addAbortHandler unlock
      found <- liftIO $ CASList.find list key
      if found
        then return False
        else do
          addAbortHandler undo
          liftIO $ CASList.addToTail list key
          return True
    else abort
  where
    undo = CASList.delete list key >> return ()
    unlock = AbstractLock.unlock alock key >> return ()

contains :: TBList.ListHandle Int -> Int -> IO Bool
contains (TBL alock list) key = atomically $ performIO $ do
  ok <- liftIO $ lock alock key
  if ok
    then do
      addCommitHandler unlock
      addAbortHandler unlock
      liftIO $ CASList.find list key
    else abort
  where
    unlock = AbstractLock.unlock alock key >> return ()

remove :: TBList.ListHandle Int -> Int -> IO Bool
remove (TBL alock list) key = atomically $ performIO $ do
  ok <- liftIO $ lock alock key
  if ok
    then do
      addCommitHandler unlock
      addAbortHandler unlock
      removed <- liftIO $ CASList.delete list key
      if removed
        then addAbortHandler undo >> return removed
        else return removed
    else abort
  where
    undo = CASList.addToTail list key
    unlock = AbstractLock.unlock alock key >> return ()
