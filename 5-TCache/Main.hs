{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-} 
module Main where
-------------------------------------------------
-- A example of Transactional cache usage (TCache.hs)
-- (Something like the Java Hibernate)
-- Author: Alberto G�ez Corona Nov 2006
-- Language: Haskell
-- Terms of use: you can do whatever you want
-- with this code as long as you keep this notice
------------------------------------------------

import Data.TCache

import Control.Concurrent
import System.Directory
import Control.Monad
import GHC.Conc
import System
import System.Time
import Text.Printf

--1 and 4: The data elements to be used in the example: A user will repeatedly buy Items.

data  Data=   User{uname::String, uid::String, spent:: Int} |
              Item{iname::String, iid::String, price::Int, stock::Int}
 
              deriving (Read, Show)

--3 The mappings between the cache and the phisical storage are defined by the interface IResource

--      to extract the resource unique key,

--      to read the resource from the physical storage,

--      to store it and

--      to delete the resource from the physical storage.


  

instance IResource Data where
        keyResource         User{uid=id}= id
        keyResource         Item{iid=id}= id        
        readResource    e= do s<- readFile$ keyResource e
                              return$ Just $ read s                                   
        writeResource   e= writeFile (keyResource e) $ show e

        delResource     e= removeFile $ keyResource e



-- buy is the operation to be performed in the example

--4 withResources gets a partial definition of each resource necessary for extracting the key, fill all the rest of the data structures (if found ) and return a list of Maybe Data. BuyIt is part of the domain problem. it receive this list and generates a new list of dat objects that are updated in the cache. buyIt is executed atomically.

 
user `buy` item=
    	withResources[user,item] buyIt

	where
	

	buyIt[Just us,Just it]
		| stock it > 0= [us',it'] -- `debug1` ((uname us) ++ " buys a PC")
		| otherwise   = error "stock is empty for this product"

		where 	
		us'= us{spent=spent us + price it}
		it'= it{stock= stock it-1}
	buyIt[_,_] = error "either the user or the item does not exist"
	
createThread :: Int -> String -> String -> MVar Int -> IO ThreadId
createThread numOps usr item mvar =
 	 forkIO ( do
			{ replicateM_ numOps $ User{uid=usr} `buy` Item{iid=item}
			; putMVar mvar 1
			}
          	 )

createThreads :: Int -> Int -> [String] -> String -> [MVar Int] -> IO()
createThreads 0 _ _ _ _ = return ()
createThreads n numOps users item mvars
	= do 
		{
		;  createThread numOps (users!!(n-1)) item (mvars!!(n-1))
		; createThreads (n - 1) numOps users item mvars
		}


main= do
        timeStart <- getClockTime
	args <- getArgs
	let numOps = read (args!!0)
	let numThreads = read (args!!1)
	putStr "Starting"
	let items _= [User ("Usr"++show x) ("U"++show x) 0 | x <- [1 .. numThreads]] ++ [Item "PC" "I1" 6000 numOps]

	let users = ["U"++show x | x <- [1 .. numThreads]]


	-- create resources (acces no resources and return two new Data objects defined in items)
        withResources [] items

	
        --11 PCs are charged  to the Johns account in paralel, to show transactionality
        --because there are only 10 PCs in stock, the last thread must return an error
	
	mvars <- replicateM numThreads newEmptyMVar
	createThreads numThreads (div numOps numThreads) users "I1" mvars
	mapM_ takeMVar mvars
        --wait 5 seconds        
        --threadDelay 5000000

        -- write the cache content in a persistent store (invoque writeResource for each resource)
        -- in a real application clearSyncCache can be used instead to adjust size and write the cache periodically

        syncCache (refcache :: Cache Data)

	--syncCache (DefaultPersistResource refcache)
	

        timeEnd <- getClockTime
        let diff = (normalizeTimeDiff (diffClockTimes timeEnd timeStart))
        --readTStats
        putStr "RunTime:"	
        print ((((fromIntegral(tdPicosec diff))/(10^12))) + (fromIntegral((tdSec diff) + (60 * (tdMin diff)) + (3600 * (tdHour diff)))))

        -- the files have been created. the files U12345 and I54321 must contain the result of the 11 iterations
