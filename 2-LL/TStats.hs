module TStats
  ( TStats
  , atomicWithStats
  , atomicAndPrintStats
  , printStats
  ) where

import GHC.Conc.Sync

type TStats a = (a    -- | Value returned by the transaction
                , Int -- | Number of full aborts
                , Int -- | Number of parcial aborts
                , Int -- | Number of full transaction retrys
                , Int -- | Number of parcial transaction retrys
                )

-- | Runs a transaction and returns some statistics
atomicWithStats :: STM a -> IO (TStats a)
atomicWithStats t = atomically $ do
  v  <- t
  a  <- readTAborts
  na <- readTNestedAborts
  r  <- readTRetrys
  nr <- readTNestedRetrys
  return (v, a, na, r, nr)

printStats :: TStats a -> IO a
printStats (v, a, na, r, nr) = do
  print $ "Number of full aborts: " ++ (show a)
  print $ "Number of parcial aborts: " ++ (show na)
  print $ "Number of full transaction retrys: " ++ (show r)
  print $ "Number of parcial transaction retrys: " ++ (show nr)
  return v

atomicAndPrintStats :: STM a -> IO a
atomicAndPrintStats t = do
  (v, a, na, r, nr) <- atomicWithStats t
  print $ "Number of full aborts: " ++ (show a)
  print $ "Number of parcial aborts: " ++ (show na)
  print $ "Number of full transaction retrys: " ++ (show r)
  print $ "Number of parcial transaction retrys: " ++ (show nr)
  return v
