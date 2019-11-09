-- Nehir and Cristian's implementation of ordered linked lists.

 module BinaryTree where
 import Control.Concurrent.STM.TVar
 import GHC.Conc
 import Control.Exception
 import System.Random as Random
 import System.IO.Unsafe

 data Tree
  = Node { val :: Int,
           right :: TVar Tree,
	   left :: TVar Tree }
    | Nil
	deriving (Eq)

 newTreeNode :: Int -> Tree -> Tree -> STM (Tree)
 newTreeNode number left right  
	= do
	    {  leftTNode <- newTVar left
	    ;  rightTNode <- newTVar right
	    ;  let result = Node {val = number, right = rightTNode, left = leftTNode} 
	    ;  return (result)
	    }

 createSampleTree :: [Int] -> TVar Tree -> STM ()
 createSampleTree [] _ = return ()
 createSampleTree theList tTree = 
	do
	{ insertTree tTree centralElem
	; createSampleTree firstHalf tTree
	; createSampleTree secondHalf tTree
	}
	where
		centralIndex = (length theList) `div` 2
		centralElem = theList!!centralIndex
		firstHalf = take centralIndex theList
		secondHalf = take centralIndex (reverse theList)
		

 insertTree :: TVar Tree -> Int -> STM ()
 insertTree curTNode numberToInsert = 
	do
	{ 
	  curNode <- readTVar curTNode
	; case curNode of
	  Nil -> do {
			newNode <- newTreeNode numberToInsert Nil Nil 
			; writeTVar curTNode newNode
		}
	  Node {val = value, right = rightNode, left = leftNode} ->
		if (value == numberToInsert) 
	           then return ()
		   else
			if (value < numberToInsert) 
				then (insertTree rightNode numberToInsert)
				else (insertTree leftNode numberToInsert)
	}

 findRightOfGreatestNode :: TVar Tree -> STM (TVar Tree)
 findRightOfGreatestNode curTNode =
	do
	{ curNode <- readTVar curTNode
	; if (curNode == Nil) 
		then return curTNode
		else findRightOfGreatestNode (right curNode)
	}

 combine :: TVar Tree -> TVar Tree -> STM (Tree)
 combine rightTNode leftTNode =
	do
	{ rightNode <- readTVar rightTNode
	; leftNode <- readTVar leftTNode
	; if (leftNode == Nil) 
		then return rightNode
		else do 
			{ greatestNode <- findRightOfGreatestNode leftTNode
			; writeTVar greatestNode rightNode
			; return leftNode
			}
	}


 deleteTree :: TVar Tree -> Int -> STM ()
 deleteTree curTNode numberToDelete =
	do
	{
	  curNode <- readTVar curTNode
	; case curNode of
	  Nil -> return ()
	  Node {val = value, right = rightNode, left = leftNode} ->
		do 
		{ if (value == numberToDelete) 
			then 
				do 
				{ combinedTree <- combine rightNode leftNode
				; writeTVar leftNode Nil
				; writeTVar rightNode Nil
				; writeTVar curTNode combinedTree
				}
			else
			   if (value < numberToDelete) 
				then (deleteTree rightNode numberToDelete)
				else (deleteTree leftNode numberToDelete)
		}
	}

 lookupTree :: TVar Tree -> Int -> STM ()
 lookupTree curTNode numberToLookup =
	do
	{
	  curNode <- readTVar curTNode
	; case curNode of
	  Nil -> return ()
	  Node {val = value, right = rightNode, left = leftNode} ->
		do 
		{ if (value == numberToLookup) then do { return() }
			else
			   if (value < numberToLookup) 
				then (lookupTree rightNode numberToLookup)
				else (lookupTree leftNode numberToLookup)
		}
	}

 toStringNode :: Tree -> STM String
 toStringNode Nil = return "Nil"
 toStringNode Node {val = value} = return (show value)

 toString :: Tree -> STM String
 toString Nil = return ""
 toString Node {val = value, right = rightNode, left = leftNode}
	= do
	{ theRightNode <- readTVar rightNode
	; theLeftNode <- readTVar leftNode
	; restRight <- toString theRightNode
	; restLeft <- toString theLeftNode
	; leftNodeString <- toStringNode theLeftNode
	; rightNodeString <- toStringNode theRightNode
	; return (leftNodeString++"<-" ++ (show value) ++ "->" ++ rightNodeString ++"\n" ++ restRight ++ restLeft)
	}


{-
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
-}

{-
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
-}


{-
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
-}
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

