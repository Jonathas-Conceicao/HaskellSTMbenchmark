module LLIOSTM where

import GHC.IO
import Data.IORef
import Data.Atomics
import Control.Concurrent
import Control.Concurrent.Chan
import System.Environment
import Control.Concurrent.STM

-- FIXME: remove this
import qualified GHC.Conc.Sync as Hack

data List a = Node { val :: a
                   , next :: IORef (List a) }
            | DelNode { next :: IORef (List a) }
            | Null
            | Head { next :: IORef (List a) } deriving Eq

data ListHandle a = ListHandle { headList :: IORef (IORef (List a)),
                             tailList :: IORef (IORef (List a)) }

-------------------------------------------
-- functions matching LLSTM2 api

createSampleList :: [Int] -> STM (ListHandle Int)
createSampleList l = Hack.unsafeIOToSTM $ do
  new_list <- newList
  aux new_list l
  where
    aux l [] = pure l
    aux l (x:xs) = do
      addToTail l x
      aux l xs

insertListNode :: TVar (ListHandle Int) -> Int -> STM ()
insertListNode l k = do
  l <- readTVar l
  Hack.unsafeIOToSTM $ addToTail l k

deleteListNode :: TVar (ListHandle Int) -> Int -> STM ()
deleteListNode l k = do
  l <- readTVar l
  Hack.unsafeIOToSTM $ delete l k
  return ()

toString :: ListHandle Int -> STM String
toString = error "FIXME: unimplemented"

-------------------------------------------
-- auxilliary functions

repeatUntil :: IO Bool -> IO ()
repeatUntil cmd = do { b <- cmd; if b then return ()
                                 else repeatUntil cmd }

atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
atomCAS ptr old new =
   atomicModifyIORefCAS ptr (\ cur -> if cur == old
                                   then (new, True)
                                   else (cur, False))

atomicWrite :: IORef a -> a -> IO ()
atomicWrite ptr x =
   atomicModifyIORefCAS ptr (\ _ -> (x,()))

----------------------------------------------
-- functions operating on lists

newList :: IO (ListHandle a)
newList =
   do null <- newIORef Null
      hd <- newIORef (Head {next = null })
      hdPtr <- newIORef hd
      tailPtr <- newIORef null
      return (ListHandle {headList = hdPtr, tailList = tailPtr})

addToTail :: Eq a => ListHandle a -> a -> IO ()
addToTail (ListHandle {tailList = tailPtrPtr}) x =
   do null <- newIORef Null
      repeatUntil
         (do tailPtr <- readIORef tailPtrPtr
             b <- atomCAS tailPtr Null (Node {val = x, next = null})
             return b )
        -- we atomically update the tail
        -- (by spinning on the tailPtr)
      atomicWrite tailPtrPtr null


find :: Eq a => ListHandle a -> a -> IO Bool
find (ListHandle { headList = head }) x =
  let go prevPtr =
        do do prevNode <- readIORef prevPtr
              let curPtr = next prevNode -- head/node/delnode have all next
              curNode <- readIORef curPtr
              case curNode of
                Node {val = y, next = nextNode } ->
                   if (x == y)
                   then -- node found and alive
                      return True
                   else go curPtr -- continue
                Null -> return False -- reached end of list
                DelNode {next = nextNode } ->
                         -- atomically delete curNode by setting the next of prevNode to next of curNode
                         -- if this fails we simply move ahead
                        case prevNode of
                          Node {} -> do b <- atomCAS prevPtr prevNode (Node {val = val prevNode,
                                                                             next = nextNode})
                                        if b then go prevPtr
                                         else go curPtr
                          Head {} -> do b <- atomCAS prevPtr prevNode (Head {next = nextNode})
                                        if b then go prevPtr
                                         else go curPtr
                          DelNode {} -> go curPtr    -- if parent deleted simply move ahead
             {-
                correct as well, but a deleted parent deleting a child is (for certain cases) a useless operation
                                     do atomicModifyIORef prevPtr ( \ cur -> (cur{next = nextNode},True))
                                        go prevPtr
              -}

  in do startPtr <- readIORef head
        go startPtr

delete :: Eq a => ListHandle a -> a -> IO Bool
delete (ListHandle { headList = head }) x =
  let go prevPtr =
        do do prevNode <- readIORef prevPtr
              let curPtr = next prevNode -- head/node/delnode have all next
              curNode <- readIORef curPtr
              case curNode of
                Node {val = y, next = nextNode } ->
                   if (x == y)
                   then -- node found and alive
                      do b <- atomCAS curPtr curNode (DelNode {next = nextNode})
                         if b then return True
                          else go prevPtr -- spin
                   else go curPtr -- continue
                Null -> return False -- reached end of list
                DelNode {next = nextNode } ->
                         -- atomically delete curNode by setting the next of prevNode to next of curNode
                         -- if this fails we simply move ahead
                        case prevNode of
                          Node {} -> do b <- atomCAS prevPtr prevNode (Node {val = val prevNode,
                                                                             next = nextNode})
                                        if b then go prevPtr
                                         else go curPtr
                          Head {} -> do b <- atomCAS prevPtr prevNode (Head {next = nextNode})
                                        if b then go prevPtr
                                         else go curPtr
                          DelNode {} -> go curPtr    -- if parent deleted simply move ahead

  in do startPtr <- readIORef head
        go startPtr

printList :: Show a => ListHandle a -> IO ()
printList (ListHandle {headList = ptrPtr}) =
  do startptr <- (
          do ptr <- readIORef ptrPtr
             Head {next = startptr} <- readIORef ptr
             return startptr)
     printListHelp startptr

printListHelp :: Show a => IORef (List a) -> IO ()
printListHelp curNodePtr =
   do { curNode <- readIORef curNodePtr
      ; case curNode of
          Null -> putStr "Nil"
          Node {val = curval, next = curnext} ->
             do { putStr (show curval  ++ " -> ")
                ;  printListHelp curnext }
          DelNode {next = curnext} ->
             do { putStr ("DEAD -> ")
                ;  printListHelp curnext }
      }

