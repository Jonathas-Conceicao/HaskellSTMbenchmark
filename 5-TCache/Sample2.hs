 module Main where
-------------------------------------------------
-- A example of Transactional cache usage (TCache.hs)
-- (Something like the Java Hibernate)
-- Author: Alberto Gómez Corona Nov 2006
-- Language: Haskell
-- Terms of use: you can do whatever you want
-- with this code as long as you keep this notice
------------------------------------------------

import Data.TCache

import Control.Concurrent
import Debug.Trace

debug a b= trace b a

--1 and 4: The data elements to be used in the example

data  Data=   Data Int String
 
              deriving (Read, Show)

--3 The mappings between the cache and the phisical storage are defined by the interface IResource

--      to extract the resource unique key, required

--      to serialize/deserialize it into/from a String, required

--      to define the prefix used for default persistence in files (if prefix have "/", a folder will be created)

-- OPTINONAL:

--      to read the resource from the physical storage, (default provided in file)

--      to store it  (default provided)

--      to delete the resource from the physical storage. (default provided)

 

instance IResource Data where
        keyResource         (Data i _)= show i   
        serialize x= show x
        deserialize x= read x
        defPath _ = "data/"  -- directory where the data is stored.
        
        -- other definable methods: readResource, writeResource delResource. here the default persistence in files are used
        
-- buy is the operation to be performed in the example

--4 withResources gets a partial definition of each resource necessary for extracting the key, 
--fill all the rest of the data structures (if found ) and return a list of Maybe Data. 
--BuyIt is part of the domain problem. it receive this list and generates a new list of 
--data objects that are updated in the cache. buyIt is executed atomically.





main= do
        --asyncronous write every 10 seconds, 100 elems max cache
        clearSyncCacheProc (refcache :: Cache Data) 10 defaultCheck 100 
        
        -- create resources (
        -- (acces no resources and return two new Data objects defined in items)
        withResources[] (\_ ->[Data i "olddata" | i <-[1..200]])
              
        -- after 10 seconds, 200 files  have been created in the folder "data"
        -- because 200 exceeds the maximum cache size (100) defaultCheck will discard 
        -- then 150 first and older elems from cache to reduce it to a half.
        
        
        -- wait 20 seconds 
        
        threadDelay 20000000
        
        
        print "modifyng"
        --update all the list. discarded elems from cache are also updated atomically
        withResources[]  (\_ ->[Data i "newdata" | i <-[1..200]]) 
        --state in disk is also atomically written.
        
       
        
        threadDelay 20000000
       
        



