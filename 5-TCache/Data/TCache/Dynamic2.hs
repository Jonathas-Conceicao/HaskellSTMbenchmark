{-# OPTIONS -fglasgow-exts  -fallow-undecidable-instances -fbang-patterns #-}

-- IDynamic: an Extensible, serializable, T.IResource datatype,
-- to be used within any container in order to handle, store and retrieve
-- heterogeneous datatypes defined in different modules.

module Data.TCache.Dynamic(
  T.IResource(..),
  Register (
    toDyn           -- :: x -> IDynamic
    ,registerType   -- :: x -> IO()
    ,fromDyn        -- :: IDynamic -> x
    ,unsafeFromDyn  -- :: IDynamic -> x
))

where
import System.IO.Unsafe
import Control.Concurrent.MVar
import Data.Typeable
import Unsafe.Coerce
import qualified Data.TCache as T
import Debug.Trace



debug a b= trace b a 

type IDynamic= forall a. (Typeable a, T.IResource a) =>   a 



type Deserializer =   (String, (String -> IDynamic ))
list :: MVar [fromStringr]

list = unsafePerformIO $ newMVar [] 


instance  T.IResource IDynamic  where
   keyResource (  x)= T.keyResource x
   serialize (  x)= show (typeOf x) ++ "\n"++ T.serialize x
   deserialize str= case lookup  key (unsafePerformIO $ readMVar list) of
                           Nothing ->   error $ "not registered type "++key++" please registerType it"
                           Just f  ->  unsafeCoerce $ f tail  
                                                      
     where
     (key, tail)= span (/='\n') str


instance Show IDynamic where
 show x= T.serialize x
  

class Register  x where 
 toDyn :: x -> IDynamic
 registerType ::  x -> IO()
 fromDyn :: IDynamic -> x
 unsafeFromDyn :: IDynamic -> x
 -- get(toDyn x)== x

instance (T.IResource x,Typeable x) => Register  x where
 toDyn x= unsafeCoerce  x
 
 registerType  x = do
       let f= T.deserialize :: (String -> x)        
       let f1 s= unsafeCoerce  (f s) 
       l <- takeMVar list
       putMVar list $ (show $ typeOf x ,f1):l
       
 fromDyn (  a)= if type2 == type1 then v
                        else error ("fromDyn: casting "++ "from type "++ T.serialize a ++" to type "++show type2)
           where 
           v=  unsafeCoerce a :: x
           type1= typeOf a
           type2= typeOf v
 
 unsafeFromDyn (  a)= unsafeCoerce a
 

withDResource :: IDynamic->(Maybe IDynamic->IDynamic)->IO ()
withDResource =  T.withResource 

withDResources:: [IDynamic]->([Maybe IDynamic]->[IDynamic])->IO ()
withDResources =  T.withResources 
withDResourcesID :: [IDynamic]->([Maybe IDynamic]->[T.Operation IDynamic])->IO ()
withDResourcesID =  T.withResourcesID 

getDResource :: IDynamic ->  IO (Maybe IDynamic)
getDResource  = T.getResource 

getDResources :: [IDynamic] ->  IO [Maybe IDynamic]
getDResources = T.getResources

-- return error if any resource is not found
justGetDResources rs=do mrs <- getDResources rs
                        return $ map process $ zip mrs rs  
        where
            process (Nothing, r) = error ("\""++T.keyResource r ++ "\" does not exist")
            process (Just r', _) = r'
    
justGetDResource r= do  [r']<- justGetDResources [r]
                        return r'

                 

deleteDResource :: IDynamic -> IO ()
deleteDResource= T.deleteResource


syncCache=   T.syncCache (T.refcache :: T.Cache IDynamic) 
                 
                

clearSyncCacheProc= T.clearSyncCacheProc (T.refcache :: T.Cache IDynamic)




withResource ::(Typeable a, T.IResource a) => a->(Maybe a->a)->IO ()
withResource r f=  withResources [r] (\[mr]-> [f mr])

withResources::(Typeable a, T.IResource a) => [a]->([Maybe a]->[a])->IO ()
withResources rs f=  withDResources (map toDyn rs) (\mrs-> f' mrs) where
          f' = map toDyn . f . map g
          g Nothing= Nothing
          g (Just x)= Just (fromDyn x)
          

withResourcesID :: (Typeable a, T.IResource a) => [a]->([Maybe a]->[T.Operation a])->IO ()
withResourcesID rs f=  withDResourcesID (map toDyn rs) (\mrs-> f' mrs) where
          f' = map h . f . map  g
          g Nothing= Nothing
          g (Just x)= Just (fromDyn x)
          h (T.Insert x)= T.Insert $ toDyn x
          h (T.Delete x)= T.Delete $ toDyn x
          
          
getResource ::(Typeable a, T.IResource a) => a ->  IO (Maybe a)
getResource  x= getDResource (toDyn x) >>= return . g where 
          g Nothing= Nothing
          g (Just x)= Just (fromDyn x)
          
getResources ::(Typeable a, T.IResource a) => [a] ->  IO [Maybe a]
getResources rs = getDResources (map toDyn rs) >>= return . map g where
          g Nothing= Nothing
          g (Just x)= Just (fromDyn x)

                 

deleteResource ::(Typeable a, T.IResource a) => a -> IO ()
deleteResource x= deleteDResource (toDyn x)




                 
                





-------------- tests---------

instance T.IResource Int where
   keyResource _= "I"
   serialize = show
   deserialize = read
  
  
 
instance T.IResource String where
   keyResource _= "S"
   serialize = show
   deserialize = read




test= do

  registerType (1::Int)
  registerType ("hola")
  
 
  withResources  []  (\_->[1 ::Int, 1::Int])
  withResources  []  (\_->[ "hola", "hola"])
  
  syncCache 
  
  res <- getResources [1::Int,  1::Int]
  print res
  
  res <- getResources ["hola", "hola"]
  print res
  
  res <- getDResources [toDyn "hola", toDyn ( 1::Int)]
  print res
  
