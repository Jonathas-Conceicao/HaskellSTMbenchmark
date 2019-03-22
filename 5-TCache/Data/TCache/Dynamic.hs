{-# OPTIONS -fglasgow-exts  -fallow-undecidable-instances -fbang-patterns #-}

-- Data.TCache.Dynamic:
-- a dynamic interface for TCache
 
module Data.TCache.Dynamic(
  T.IResource(..)   -- from TCache
  ,T.Operation(..)
  ,T.setCache
  ,T.refcache
  ,T.defaultCheck,T.readFileStrict
  ,IDynamic(..)        -- serializable/indexable existential datatype
  ,T.Cache
  ,DynamicInterface (   
    toIDyn           -- :: x -> IDynamic
    ,registerType   -- :: x -> IO()          
    ,fromIDyn        -- :: IDynamic -> x
    ,unsafeFromIDyn  -- :: IDynamic -> x
   )
  ,Key(..)            {- Key datatype can be used to read any object trough the Dynamic interface

                          let key= <key of the object >
                          mst <- getDResource $ Key key
                          case mst of
                           Nothing -> error $ "getResource: not found "++ key 
                           Just (idyn) -> do
                             let st = fromIDyn idyn :: <desired datatype>
                             ....
                     -}

-- same access interface , this time for Dynamic type. See Data.TCache for their equivalent definitions
,getDTVars,withDResource, withDResources, withDResourcesID, getDResource, getDResources, deleteDResource, deleteDResources


-- syncache has no parameters now (see Data.TCache.syncCache)
,syncCache

-- Same than Data.TCache but without Cache parameter
,clearSyncCacheProc

-- the same interface for any datatype:
, withResource, withResources, withResourcesID, getResource, getResources, deleteResource, deleteResources


)

where
import System.IO.Unsafe
import Control.Concurrent.MVar
import Data.Typeable
import Unsafe.Coerce
import qualified Data.TCache as T
import Debug.Trace
import Control.Concurrent.STM(TVar)
import Unsafe.Coerce



debug a b= trace b a 

data IDynamic= forall a. (Typeable a, T.IResource a) => IDynamic  a deriving Typeable



type Deserializer =   (String, (String -> IDynamic ))
list :: MVar [fromStringr]

list = unsafePerformIO $ newMVar [] 

instance  T.IResource IDynamic  where
   keyResource (IDynamic  x)= T.keyResource x
   defPath (IDynamic x)= T.defPath x
   serialize (IDynamic  x)= show (typeOf x) ++ "\n"++ T.serialize x
   deserialize str= case lookup  key (unsafePerformIO $ readMVar list) of
                           Nothing ->   error $ "not registered type "++key++" please registerType it"
                           Just f  ->   f (tail objstr)  
                                                      
     where
     (key, objstr)= span (/='\n') str


instance Show IDynamic where
 show (IDynamic x)= "(IDynamic \""++show (typeOf x) ++"\" "++  T.serialize x++")"
  

class DynamicInterface  x where 
 toIDyn :: x -> IDynamic
 registerType ::   IO x
 fromIDyn :: IDynamic -> x
 unsafeFromIDyn :: IDynamic -> x
 -- get(toIDyn x)== x

instance (T.IResource x,Typeable x) => DynamicInterface  x where
 toIDyn x= IDynamic  x
 
 registerType = do
       let x= unsafeCoerce 1 :: x
       let f= T.deserialize :: (String -> x)        
       let f1 s= IDynamic  (f s) 
       l <- takeMVar list
       case lookup (strType x) l of
         Just _ -> do
                   putMVar list l
                   return x
         _      -> do
                   putMVar list $ (strType x ,f1):l
                   return x
       where
       strType x= show $ typeOf x
       
 fromIDyn d@(IDynamic  a)= if type2 == type1 then v
                        else error ("fromIDyn: casting "++ show type1 ++" to type "++show type2 ++" for data "++ T.serialize a)
           where 
           v=  unsafeCoerce a :: x
           type1= typeOf a
           type2= typeOf v
 
 unsafeFromIDyn (IDynamic  a)= unsafeCoerce a
 
{- Key datatype can be used to read any object trough the Dynamic interface

                          let key= <key of the object >
                          mst <- getDResource $ Key key
                          case mst of
                           Nothing -> error $ "getResource: not found "++ key 
                           Just (idyn) -> do
                             let st = fromIDyn idyn :: <desired datatype>
-}

data Key= Key String deriving Typeable
instance T.IResource Key where
  keyResource (Key k)=k
  serialize (Key x)= x
  deserialize str= Key str

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

getDTVars ::  [IDynamic] -> IO [Maybe (TVar IDynamic)]
getDTVars= T.getTVars

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

deleteDResources :: [IDynamic] -> IO ()
deleteDResources= T.deleteResources

syncCache=   T.syncCache (T.refcache :: T.Cache IDynamic) 
                 
                

clearSyncCacheProc= T.clearSyncCacheProc (T.refcache :: T.Cache IDynamic)

withResource ::(Typeable a, T.IResource a) => a->(Maybe a->a)->IO ()
withResource r f=  withResources [r] (\[mr]-> [f mr])

withResources::(Typeable a, T.IResource a) => [a]->([Maybe a]->[a])->IO ()
withResources rs f=  withDResources (map toIDyn rs) (\mrs-> f' mrs) where
          f' = map toIDyn . f . map g
          g Nothing= Nothing
          g (Just x)= Just (fromIDyn x)
          

withResourcesID :: (Typeable a, T.IResource a) => [a]->([Maybe a]->[T.Operation a])->IO ()
withResourcesID rs f=  withDResourcesID (map toIDyn rs) (\mrs-> f' mrs) where
          f' = map h . f . map  g
          g Nothing= Nothing
          g (Just x)= Just (fromIDyn x)
          h (T.Insert x)= T.Insert $ toIDyn x
          h (T.Delete x)= T.Delete $ toIDyn x
          
          
getResource ::(Typeable a, T.IResource a) => a ->  IO (Maybe a)
getResource  x= getDResource (toIDyn x) >>= return . g where 
          g Nothing= Nothing
          g (Just x)= Just (fromIDyn x)
          
getResources ::(Typeable a, T.IResource a) => [a] ->  IO [Maybe a]
getResources rs = getDResources (map toIDyn rs) >>= return . map g where
          g Nothing= Nothing
          g (Just x)= Just (fromIDyn x)

                 

deleteResource ::(Typeable a, T.IResource a) => a -> IO ()
deleteResource x= deleteDResource (toIDyn x)

deleteResources ::(Typeable a, T.IResource a) => [a] -> IO ()
deleteResources xs= deleteDResources (map toIDyn xs)



                 
                



  
