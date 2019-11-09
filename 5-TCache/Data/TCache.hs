{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}


-------------------------------------------------
-- A Transactional data cache with configurable persitence
-- (Something like a little Java Hybernate or Rails for Rubi)
-- Author: Alberto Gómez Corona Nov 2006
-- Language: Haskell
-- Version: 0.5
-- Terms of use: See LICENSE
-- 2008:
-- some bugs fixed
-- 10/15/2007 : changes
-- Default writeResource and delResource for persistence in files
--     (only keyResource must be defined by the user if use defaults)
-- Coherent Inserts and deletes
-- Reduced the number of accesses to the hashtable
-- hashtable access put outside of the transaction block (takeBlocks) 
--    faster re-executions in case of roll-back

------------------------------------------------

 
module Data.TCache (

 IResource(..)                        -- class interface to be implemented for the object by the user

,Operation (Insert,Delete) -- data definition used to communicate object Inserts and Deletes to the cache

,Cache            -- :: IORef (Ht a,Int, Integer)     --The cache definition 

 ,setCache        -- :: Cache a -> IO()   -- set the cache. this is useful for hot loaded modules that will use an existing cache

,getTVars        -- :: (IResource a)=> [a]           -- the list of resources to be retrieved
                  --    -> IO [Maybe (TVar a)]        -- The Transactional variables (See Data.TVar documentation)
                  
,withResourcesID  -- :: (IResource a)=> [a]->         --list of resources to be extracted for the user function
                 --    ([Maybe a]->[Operation a])    --user function that get the retrieved resources
		 --    ->IO ()                       --and return a list of  objects to be inserted/modified or deleted

,withResources   -- :: (IResource a)=> [a]            --list of resources to be retrieve
                 --   ->([Maybe a]->[a])             ----function that get the retrieved resources
                 --   ->IO ()                        --and return a list of  objects to be inserted/modified 

,withResource    -- :: (IResource a)=> a              --same as withResources , but for one only object
                 --   ->([Maybe a]->a)               --
                 --   ->IO ()                        --

,getResources    -- :: (IResource a)=>[a]             --resources [a] are read from cache and returned
                 --   -> IO [Maybe a]   

,getResource     -- :: :: (IResource a)=>a            --to retrieve one object instead of a list
                 --   -> IO [Maybe a]   

,deleteResources -- :: (IResource a)=>[a]-> IO()      -- delete the list of resources from cache and from persistent storage
,deleteResource  -- :: (IResource a)=>a-> IO()        -- delete the  resource from cache and from persistent storage
--cache handling

,refcache        -- :: Cache a                        --the reference to the cache (see data definition below)

,syncCache       -- :: (IResource a) =>Cache a -> IO() --force the atomic write of all the cache objects into permanent storage
                                                       --useful for termination

--start the thread that clean and writes on the persistent storage trough syncCache
,clearSyncCacheProc  -- :: (IResource a) =>Cache a       --The cache reference                        
                     --   -> Int                         --number of seconds betwen checks
                     --   -> (Integer->Integer-> Bool)   --The user-defined check-for-cleanup-from-cache for each object 
                                                         --(when True, the object is removed from cache)
                     --   -> Int                         --The max number of objects in the cache, if more, the cleanup start
                     --   -> >IO ThreadId                --Identifier of the thread created

-- the default check procedure
,defaultCheck    -- ::  Integer                        -- current time in seconds
                 --   ->Integer                        --last access time for a given object
                 --   ->Integer                        --last cache syncronization (with the persisten storage)
                 --   ->Bool                           --return true for all the elems not accesed since  
                                                       --half the time between now and the last sync
                                                     
-- auxiliary
,readFileStrict  -- :: String -> IO String            -- Strict file read, needed for the default file persistence                                                                    


)
where

import GHC.Conc
import Control.Exception as Exception
import Control.Concurrent
import Control.Monad(when)
import Data.HashTable.ST.Basic as H hiding (mapM_)
import Data.IORef
import System.IO
import System.IO.Unsafe
import System.IO.Error
import System.Time 
import Data.Maybe(catMaybes,mapMaybe)
import Debug.Trace
import System.Directory
import Data.List(elemIndices)


debug1 a b= trace b a


type Key= String



-- Interface that must be defined for every object being cached
-- readResource and writeResource implemented by default as read/write files with its key as filename
-- serialize and deserialize are specified just to allow these defaults


	
class IResource a where
        serialize :: a -> String
        deserialize :: String -> a
        keyResource :: a -> String
        
        defPath :: a-> String  --additional extension for default file paths or key prefixes 
        defPath _ = "" 

	-- get object content from the file 
	-- (NOTE: reads and writes can't collide, so they-- Not really needed since no write is done while read
	-- must be strict, not lazy )
	readResource :: a->IO (Maybe a)
        readResource x=handleJust Exception.ioError (handle x) $ do 
             s::String <- readFileStrict  filename  --`debug` ("read "++filename)
             return $ Just $ deserialize s
             where
             filename=  defPath x++ keyResource x
             handle :: a -> IOError -> IO (Maybe a)
             handle x e
              |isAlreadyInUseError e = readResource x    -- maybe is being written. try again. 
                                                         
              | isDoesNotExistError e = return Nothing
              | isPermissionError e = error $ "no permissions for opening file: "++filename
              | otherwise= error $ "unspecified error: " ++ show e

	writeResource:: a->IO()
        writeResource x= handleJust Exception.ioError (handle x) $ writeFile filename (serialize x) --`debug` ("write "++filename)
             where
             filename= (defPath x ++ keyResource x)
             handle :: a -> IOError -> IO ()
             handle x e
               | isDoesNotExistError e=do
                          createDirectoryIfMissing True $ take (1+(last $ elemIndices '/' filename)) filename   --maybe the path does not exist
                          writeResource x                
               | isAlreadyInUseError e= writeResource x -- maybe is being read. try again
                                                           -- Not really needed since no write is done while read
               | isPermissionError e = error $ "no permissions for writing file: "++ filename

               | otherwise= error $ show e
               
	delResource:: a->IO()
	delResource x= handleJust Exception.ioError (handle x) $ removeFile $ defPath x ++ keyResource x
             where
             handle :: a -> IOError -> IO ()
             handle x e
               | isDoesNotExistError e= return ()
               | otherwise = error (show e)

-- to allow not only inserts but also deletes
data Operation b= Insert b | Delete b

	
type AccessTime= Integer
type ModifTime = Integer

type Block a=  (TVar a,AccessTime,ModifTime)
data Ht a= HashTable String (Block a)
-- contains the hastable, number of items, last sync time
type Cache a= IORef (Ht a, Integer)
data CheckBlockFlags= AddToHash | NoAddToHash | MaxTime
-- the cache holder

ref2cache :: IORef (Maybe (Cache a))
ref2cache= unsafePerformIO $ newIORef Nothing 
-- set the cache. this is useful for hot loaded modules that will update an existing cache
setCache :: Cache a -> IO()
setCache ch= writeIORef ref2cache  $ Just ch

refcache :: Cache a 
refcache =unsafePerformIO $do
          n <- readIORef ref2cache
          case n of
             Nothing -> do 
                          c <-  H.new (==) hashString
	                  newIORef (c,0)
             Just ch -> return ch

getTVars :: (IResource a)=> [a] -> IO [Maybe (TVar a)]
getTVars rs= do
    (cache,_) <- readIORef refcache
    takeBlocks rs cache MaxTime
	
withResourcesID:: (IResource a)=> [a]->([Maybe a]->[Operation a])->IO ()
withResourcesID rs f=  do
	(cache,_) <- readIORef refcache
        mtrs  <- takeBlocks rs cache NoAddToHash
	idrs <- atomically $ do
	    mrs <- mapM mreadTVar mtrs
	    let idrs= f mrs
            let ladd= map selectAdd idrs
            releaseBlocks (catMaybes ladd) cache
            return idrs

        let ldel= map selectDelete idrs
        let ldel1= catMaybes ldel
	mapM delResource ldel1
	delListFromHash cache  $ map keyResource ldel1
	    
	return ()

        where

	selectDelete (Insert a)= Nothing
	selectDelete (Delete a)= Just a
	
	selectAdd (Insert a)= Just a
	selectAdd (Delete a)= Nothing

mreadTVar (Just tvar)= do r <-readTVar tvar
                          return $ Just r
mreadTVar Nothing    =    return Nothing
 



withResource:: (IResource  a)=> a->(Maybe a->a)->IO ()
withResource r f= withResources [r] (\[mr]-> [f mr])




withResources:: (IResource a)=> [a]->([Maybe a]->[a])->IO ()
withResources rs f=  do
	(cache,_) <- readIORef refcache 
        mtrs  <-   takeBlocks rs cache NoAddToHash 
         
        
	atomically $ do 
	    
            mrs <- mapM mreadTVar mtrs
	    let rs'=  f mrs
	    releaseBlocks rs' cache  
	
	return ()

takeBlocks :: (IResource a)=> [a] -> Ht a -> CheckBlockFlags ->IO [Maybe (TVar a)]
takeBlocks rs cache addToHash=  mapM (checkBlock cache addToHash)  rs  
   where
   checkBlock :: IResource a =>  Ht a -> CheckBlockFlags -> a-> IO(Maybe (TVar a))
   checkBlock cache flags r =do
	c <-  H.lookup cache keyr
	case c of
		Nothing   -> do
                  mr <- readResource r                       -- `debug1` ("read "++keyr++ " hash= "++ (show $ H.hashString  keyr))
		  case mr of
                    Nothing -> return Nothing
	            Just r2 -> do 
                        tvr <- atomically $ newTVar r2  
			case flags of
                           NoAddToHash -> return $ Just tvr
                           AddToHash   -> do 
                                    ti  <-  timeInteger
			            H.insert cache keyr (tvr, ti, 0) -- accesed, not modified
                                    return $ Just tvr
                                    
                           MaxTime -> do
                                    ti  <-  timeInteger
                                    let maxtime= ti + 1000000000
                                    H.insert cache keyr (tvr, maxtime, maxtime) -- accesed, not modified
                                    return $ Just tvr
                                    
                                    

		Just(tvr,_,_)  -> return $ Just tvr

	where 	keyr= keyResource r

releaseTVars :: (IResource a)=> [a]-> IO()
releaseTVars rs=do
  (cache,_) <- readIORef refcache
  atomically $ releaseBlocks rs cache
  
releaseBlocks :: (IResource a)=> [a] -> Ht a  ->STM ()
releaseBlocks rs cache = mapM_ checkBlock  rs
    
 where
    checkBlock  r =do
	c <- unsafeIOToSTM $ H.lookup cache keyr
	case c of
	    Nothing   -> do tvr <- newTVar r  
	                    ti  <- unsafeIOToSTM timeInteger
			    unsafeIOToSTM $ H.insert cache keyr (tvr, ti, ti ) -- accesed and modified XXX
								 
				
	    Just(tvr,_,tm)  ->do  writeTVar tvr r
	                          ti  <- unsafeIOToSTM timeInteger
	                          let t=  max ti tm
				  unsafeIOToSTM $ H.insert cache keyr (tvr ,t,t)
									
					
						
	where 	keyr= keyResource r
                
                
timeInteger= do TOD t _ <- getClockTime
                return t
		         
getResource r= do{mr<-getResources [r];return $! head mr}

getResources:: (IResource a)=>[a]-> IO [Maybe a]
getResources rs= do
   (cache,_) <- readIORef refcache 
   mtrs <- takeBlocks rs cache AddToHash
   atomically $ mapM mreadTVar mtrs
		

deleteResource r= deleteResources [r]
deleteResources rs=do   
   (cache,_) <- readIORef refcache 
   atomically $! do
	unsafeIOToSTM $ mapM delResource rs
	unsafeIOToSTM $ delListFromHash cache $ map keyResource rs
        


delListFromHash  hash l= do{mapM (delete hash) l; return()}

updateListToHash hash kv= do{mapM (update1 hash) kv; return()}where
	update1 h (k,v)= insert h k v

-----------------------clear, sync cache-------------
clearSyncCacheProc ::(IResource a)=> Cache a->Int->(Integer -> Integer->Integer->Bool)->Int->IO ThreadId
clearSyncCacheProc refcache time check sizeObjects= 
  	forkIO  clear   

 where 
 clear = do
    	threadDelay $ (fromIntegral$ time * 1000000)
    	clearSyncCache refcache time check sizeObjects  
    	clear 
 

syncCache refcache = do
   (cache,_) <- readIORef refcache 
   list <- toList cache
   atomically $ save list 0 
   --print $ "write to persistent storage finised: "++ show (length list)++ " objects" 

--  - saves the unsaved elems of the cache
--  - delete some elems of  the cache when the number of elems > sizeObjects
--  - The deletion depends on the check criteria. defaultCheck is the one implemented
clearSyncCache ::(IResource a) => Cache a-> Int -> (Integer -> Integer->Integer-> Bool)-> Int -> IO ()
clearSyncCache refcache time check sizeObjects=do
   (cache,lastSync) <- readIORef refcache 
   handle (\e->do{print e;return ()})$ do
      elems <-   toList cache
      let size=length elems
      atomically $ save elems lastSync 
      t<- timeInteger 
      when (size > sizeObjects)  (filtercache t cache lastSync elems) 
      writeIORef refcache (cache, t) 

  where
        -- delete elems from the cache according with the check criteria
	filtercache t cache lastSync elems= mapM_ filter elems 	    
	    where
		check1 (_,lastAccess,_)=check t lastAccess lastSync 

		filter ::(String,Block a)->IO Int
		filter (k,e)=  if check1 e then do{H.delete cache k;return 1} else return 0

--to drop from the cache all the elems not accesed since  half the time between now and the last sync
defaultCheck:: Integer -> Integer->Integer->Bool
defaultCheck  now lastAccess lastSync
	| lastAccess > halftime = False
	| otherwise  = True

        where 
        halftime= now- (now-lastSync) `div` 2
      
  

save:: (IResource a) => [(String, Block a)]-> Integer-> STM ()
save list lastSave= mapM_ save1 list                                           --`debug1` "saving"
  where
        save1 ::  IResource a =>(String, Block a) -> STM()
	save1(_, (tvr,_,modTime))= do 
		if modTime >= lastSave                                         --`debug1` ("modTime="++show modTime++"lastSave="++show lastSave)
		  then do
			r<-  readTVar tvr
			unsafeIOToSTM $! writeResource r                       --`debug1` ("saved " ++ keyResource r)
		  else return()





readFileStrict f = do
  h <- openFile f ReadMode
  s <- hFileSize h
  let n= fromIntegral s
  str <- readn h n
  hClose h
  return str

  where
  -- read n bytes from handle h
  readn h 0= return ""
  readn h n=do	
        str <- hGetContents h
        -- force to read n elements
        if (str !! (n-1))== '\x00' then return str else return str


