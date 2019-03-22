-- Nehir and Cristian's implementation of ordered linked lists.

 module BinaryTree where
 import GHC.IOBase
 import Control.Exception
 import Random 
 import System.IO.Unsafe

 data Tree
  = Node { val :: Int,
           right :: IORef Tree,
	   left :: IORef Tree }
    | Nil
	deriving (Eq)

 newTreeNode :: Int -> Tree -> Tree -> IO (Tree)
 newTreeNode number left right  
	= do
	    {  leftTNode <- newIORef left
	    ;  rightTNode <- newIORef right
	    ;  let result = Node {val = number, right = rightTNode, left = leftTNode} 
	    ;  return (result)
	    }

 createSampleTree :: [Int] -> IORef Tree -> IO ()
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
		

 insertTree :: IORef Tree -> Int -> IO ()
 insertTree curTNode numberToInsert = 
	do
	{ 
	  curNode <- readIORef curTNode
	; case curNode of
	  Nil -> do {
			newNode <- newTreeNode numberToInsert Nil Nil 
			; writeIORef curTNode newNode
		}
	  Node {val = value, right = rightNode, left = leftNode} ->
		if (value == numberToInsert) 
	           then return ()
		   else
			if (value < numberToInsert) 
				then (insertTree rightNode numberToInsert)
				else (insertTree leftNode numberToInsert)
	}

 findRightOfGreatestNode :: IORef Tree -> IO (IORef Tree)
 findRightOfGreatestNode curTNode =
	do
	{ curNode <- readIORef curTNode
	; if (curNode == Nil) 
		then return curTNode
		else findRightOfGreatestNode (right curNode)
	}

 combine :: IORef Tree -> IORef Tree -> IO (Tree)
 combine rightTNode leftTNode =
	do
	{ rightNode <- readIORef rightTNode
	; leftNode <- readIORef leftTNode
	; if (leftNode == Nil) 
		then return rightNode
		else do 
			{ greatestNode <- findRightOfGreatestNode leftTNode
			; writeIORef greatestNode rightNode
			; return leftNode
			}
	}


 deleteTree :: IORef Tree -> Int -> IO ()
 deleteTree curTNode numberToDelete =
	do
	{
	  curNode <- readIORef curTNode
	; case curNode of
	  Nil -> return ()
	  Node {val = value, right = rightNode, left = leftNode} ->
		do 
		{ if (value == numberToDelete) 
			then 
				do 
				{ combinedTree <- combine rightNode leftNode
				; writeIORef leftNode Nil
				; writeIORef rightNode Nil
				; writeIORef curTNode combinedTree
				}
			else
			   if (value < numberToDelete) 
				then (deleteTree rightNode numberToDelete)
				else (deleteTree leftNode numberToDelete)
		}
	}

 toStringNode :: Tree -> IO String
 toStringNode Nil = return "Nil"
 toStringNode Node {val = value} = return (show value)

 toString :: Tree -> IO String
 toString Nil = return ""
 toString Node {val = value, right = rightNode, left = leftNode}
	= do
	{ theRightNode <- readIORef rightNode
	; theLeftNode <- readIORef leftNode
	; restRight <- toString theRightNode
	; restLeft <- toString theLeftNode
	; leftNodeString <- toStringNode theLeftNode
	; rightNodeString <- toStringNode theRightNode
	; return (leftNodeString++"<-" ++ (show value) ++ "->" ++ rightNodeString ++"\n" ++ restRight ++ restLeft)
	}


{-
 deleteListNode :: IORef ListNode -> Int -> IO ()
 deleteListNode curTNode numberToDelete
	= do
	    { curNode <- readIORef curTNode
	    ; let nextNTNode = nodenextN curNode
	    ; nextNNode <- readIORef nextNTNode
            ; case nextNNode of
		  Nil -> return ()
		  Node {} ->
		   do { let nextNVal = nodeVal nextNNode
	    	       ; let nextNnextNNode = nodenextN nextNNode
                      ; nextNnextNTNode <- readIORef nextNnextNNode
	               ; if (nextNVal==numberToDelete)
	      	           then
				   do
				     {
				     ; writeIORef nextNTNode nextNnextNTNode
	                   	     }
			   else if (nextNVal>numberToDelete) 
				    then
				          return ()
	        		    else do 
					   { --unreadIORef curTNode
					   ; deleteListNode nextNTNode numberToDelete
					   }
			}
	    }
-}

{-
 findListCountSteps :: ListNode -> Int -> Int -> IO(Bool, Int)
 findListCountSteps Nil _ steps = return (False, steps)

 findListCountSteps Start {nextN = tnextN} element steps
	= do
	     { nextNNode <- readIORef tnextN
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
			    { nextNNode <- readIORef tnextN
			    ; findListCountSteps nextNNode element (steps + 1)
			    }
		      else return (False, steps)
	     }

 findList :: ListNode -> Int -> IO(Bool, Int)
 findList list element = findListCountSteps list element 1 

 createEmptyList :: IO (ListNode)
 createEmptyList 
	= do { listStartTrans <- newIORef Nil
	      ; return Start {nextN = listStartTrans}
             }
-}


{-
 insertEachAtomically :: IORef ListNode -> [Int] -> IO()
 insertEachAtomically _ [] = return ()
 insertEachAtomically list (x:xs) = 
	do
	{ atomically (insertListNode list x)
	; insertEachAtomically list xs
	}
	
				

 createSampleList :: [Int] -> IO ListNode
 createSampleList listOfInts = do { myList <- createEmptyList 
 				; myTList <- newIORef myList
			   	; mapM_ (insertListNode myTList) listOfInts
			   	; return myList}
-}
{-
 createSampleList :: [Int] -> IO ListNode
 createSampleList listOfInts = do { myList <- atomically createEmptyList 
 				; myTList <- newIORefIO myList
			   	; insertEachAtomically myTList listOfInts
				; myList <- atomically (readIORef myTList)
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

