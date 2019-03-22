{-# OPTIONS -XTypeSynonymInstances #-}
-- XTypeSynonymInstances added only to permit IResource instances for Strings
module Main where
import Data.TCache.Dynamic

{------------- tests---------
example of IDynamic usage.

-}

--very simple data:
--two objects with two different datatypes: Int and String

instance IResource Int where     
   keyResource _= "I"
   serialize = show
   deserialize = read
   defPath _= "data/"
  
  
 
instance IResource String where
   keyResource _= "S"
   serialize = show
   deserialize = read
   defPath _= "data/"




main= do

  registerType :: IO Int           -- register both datatypes (Int, and String)
  registerType :: IO String
  
  let x= 1:: Int
  withDResources  []  (\_->[toIDyn x, toIDyn "hola"])   --resource creation for the example
  
  syncCache                                    --syncCache now has no parameters (refcache is not used)
  
  res <- getResources [1::Int,  1::Int]        --getResources works as allways 
  print res
  
  res <- getResources ["hola", "hola"]         --with multiple datatypes this time
  print res
  
  res <- getDResources [toIDyn "hola", toIDyn ( 1::Int)]  -- DResource calls can manage many datatypes simultaneously
  print res
  
  mres <- getDResource $ IDynamic $ Key "S"        --Key permits to retrieve any object of any datatype
  case mres of
   Nothing -> error "not found"
   Just res -> do
     print (fromIDyn  res :: String)              -- get the String content
  
     print (fromIDyn res :: Int)                  -- error reported: casting a String object to Int
