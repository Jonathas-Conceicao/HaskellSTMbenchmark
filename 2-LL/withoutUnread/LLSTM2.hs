-- Nehir and Cristian's implementation of ordered linked lists.

 module LLSTM2 where
 import Control.Concurrent.STM.TVar
-- import Control.Concurrent
 import GHC.Conc
 import Control.Exception
 import Random 
-- import System.CPUTime
-- import Text.Printf

 import System.IO.Unsafe

 data ListNode
  = Node { val :: Int,
               nextN :: TVar ListNode}
    | Start {nextN :: TVar ListNode}
    | Nil
	deriving (Eq)

 nodeVal :: ListNode -> Int
 nodeVal Node {val = curVal}
    =  curVal
 nodeVal Nil = throw (AssertionFailed "It's not possible to find out the value of NIL")
 nodeVal Start{} = throw (AssertionFailed "It's not possible to find out the value of Start")

 nodenextN :: ListNode -> TVar (ListNode)
 nodenextN Start {nextN = tnextN}
    =  tnextN
 nodenextN Node {nextN = tnextN}
    =  tnextN
 nodenextN _ = throw (AssertionFailed "It's not possible to get to the nextN node")

 newListNode :: Int -> ListNode -> STM (ListNode)
 newListNode number nextN 
	= do
	    {  tnextN <- newTVar nextN
	    ;  let result = Node {val = number, nextN = tnextN} 
	    ;  return (result)
	    }

 insertListNode :: TVar ListNode -> Int -> STM ()
 insertListNode curTNode numberToInsert = 
	do
	{ curNode <- readTVar curTNode
	; let nextNTNode = nodenextN curNode
	; let doInsertion nextNNode =
		do 
		{ newNode <- newListNode numberToInsert nextNNode
		; writeTVar nextNTNode newNode
		}
	; nextNNode <- readTVar nextNTNode
        ; case nextNNode of
		  Nil -> doInsertion nextNNode
		  Node {val = valnextNNode, nextN = nextNnextNTNode} ->
		   do
		    { if (valnextNNode == numberToInsert)
		     	then
			   return ()
			else if (valnextNNode > numberToInsert)
			       then doInsertion nextNNode	
		               else do
					{ --unreadTVar curTNode
					; insertListNode nextNTNode numberToInsert
					}
		    }
	}
	

 deleteListNode :: TVar ListNode -> Int -> STM ()
 deleteListNode curTNode numberToDelete
	= do
	    { curNode <- readTVar curTNode
	    ; let nextNTNode = nodenextN curNode
	    ; nextNNode <- readTVar nextNTNode
            ; case nextNNode of
		  Nil -> return ()
		  Node {} ->
		   do { let nextNVal = nodeVal nextNNode
	    	       ; let nextNnextNNode = nodenextN nextNNode
                      ; nextNnextNTNode <- readTVar nextNnextNNode
	               ; if (nextNVal==numberToDelete)
	      	           then
				   do
				     {
				     ; writeTVar nextNTNode nextNnextNTNode
	                   	     }
			   else if (nextNVal>numberToDelete) 
				    then
				          return ()
	        		    else do 
					   { --unreadTVar curTNode
					   ; deleteListNode nextNTNode numberToDelete
					   }
			}
	    }

 findListCountSteps :: ListNode -> Int -> Int -> STM(Bool, Int)
 findListCountSteps Nil _ steps = return (False, steps)

 findListCountSteps Start {nextN = tnextN} element steps
	= do
	     { nextNNode <- readTVar tnextN
	     ; findListCountSteps nextNNode element steps
            }

 findListCountSteps Node {val = curElem, nextN = tnextN} element steps
	= do
	     { if (curElem == element)
		then return (True, steps)
		else
		    if (curElem < element)
		      then	
			do
			    { nextNNode <- readTVar tnextN
			    ; findListCountSteps nextNNode element (steps + 1)
			    }
		      else return (False, steps)
	     }

 findList :: ListNode -> Int -> STM(Bool, Int)
 findList list element = findListCountSteps list element 1 

 createEmptyList :: STM (ListNode)
 createEmptyList 
	= do { listStartTrans <- newTVar Nil
	      ; return Start {nextN = listStartTrans}
             }

 toString :: ListNode -> STM String
 toString Nil = return "NIL"
 toString Start {nextN = tnextN}
	= do
	{ nextNNode <- readTVar tnextN
	; rest <- toString nextNNode
	; return ("START " ++ rest)
	}
 toString (Node{val = value, nextN = tnextN})
	= do
	{ nextNNode <- readTVar tnextN
	; rest <- toString nextNNode
	; return ((show value) ++ "-" ++ rest)
	}

 insertEachAtomically :: TVar ListNode -> [Int] -> IO()
 insertEachAtomically _ [] = return ()
 insertEachAtomically list (x:xs) = 
	do
	{ atomically (insertListNode list x)
	; insertEachAtomically list xs
	}
	
				

 createSampleList :: [Int] -> STM ListNode
 createSampleList listOfInts = do { myList <- createEmptyList 
 				; myTList <- newTVar myList
			   	; mapM_ (insertListNode myTList) listOfInts
			   	; return myList}

{-
 createSampleList :: [Int] -> IO ListNode
 createSampleList listOfInts = do { myList <- atomically createEmptyList 
 				; myTList <- newTVarIO myList
			   	; insertEachAtomically myTList listOfInts
				; myList <- atomically (readTVar myTList)
			   	; return myList}

-}
{-

 insertNTimes :: ListNode -> Int -> Int -> IO()
 insertNTimes _ _ 0 = return ()
 insertNTimes aList maxNumber counter = do {rnd <- randomRIO (1::Int, maxNumber)
				             ; atomically (insertListNode aList rnd)
					     ; insertNTimes aList maxNumber (counter - 1)
                                            } 

 deleteNTimes :: ListNode -> Int -> Int -> IO()
 deleteNTimes _ _ 0 = return ()
 deleteNTimes aList maxNumber counter = do { rnd <- randomRIO (1::Int, maxNumber)
					     ;  atomically (deleteListNode aList rnd)
		                             ; deleteNTimes aList maxNumber (counter - 1)
                                            } 

 deleteAndInsertNTimes :: ListNode -> Int -> Int -> IO()
 deleteAndInsertNTimes _ _ 0 = return ()
 deleteAndInsertNTimes aList maxNumber counter = do { rnd <- randomRIO (1::Int, maxNumber)
					     		;  rnd2 <- randomRIO (1::Int, maxNumber)
					     		;  atomically (do{deleteListNode aList rnd
					     		;                 ; insertListNode aList rnd2})
		                             		; deleteAndInsertNTimes aList maxNumber (counter - 1)
                                            	     } 

-}

