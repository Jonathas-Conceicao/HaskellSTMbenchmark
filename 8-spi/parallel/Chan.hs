module Chan(
	Chan, mkChan, mkDelayChan, chanRate, isDelayChan,
	Rate, infiniteRate, isInfiniteRate,
		scaledRate, addRates,
	GblId(..), mkGblId, 
	Unique(), firstUnique, nextUnique, uniqueFrom,

	thenCmp
   ) where

import Control.Exception( assert )

-------------------------------
--	GblIds
-------------------------------

data GblId = GblId Unique String	-- Globally unique

-- Pair of a processor number (or 0 for the initial program) and 
-- a locally unique integer
type Unique = (Int, Int) 

mkGblId :: Unique -> String -> GblId
mkGblId = GblId

firstUnique :: Int -> Unique
firstUnique b = (b,1)		-- First available

uniqueFrom :: Unique -> [Unique]
uniqueFrom (i,j) = (i,j) : uniqueFrom (i,j+1)

nextUnique :: Unique -> Int -> Unique
nextUnique (i,j) n = (i,j+n)

instance Eq GblId where
   (GblId id1 _) == (GblId id2 _) = id1 == id2

instance Ord GblId where
   (GblId id1 _) `compare` (GblId id2 _) = id1 `compare` id2

instance Show GblId where
  show (GblId u s) = s -- ++ ":" ++ show u	


-------------------------------
--	Channels
-------------------------------

data Chan = Chan { chan_id :: GblId, 
		   chan_rate :: Rate,
		   chan_approx :: Int 	-- Aproximate rate
		}
	  | DelayChan { chan_rate :: Rate }

chanRate :: Chan -> Rate
chanRate = chan_rate

mkChan :: GblId -> Rate -> Chan
mkChan id r = Chan { chan_id = id, chan_approx = round r, chan_rate = r }

mkDelayChan :: Rate -> Chan
mkDelayChan r = DelayChan { chan_rate = r }

isDelayChan :: Chan -> Bool
isDelayChan (DelayChan {}) = True
isDelayChan (Chan {})      = False

instance Eq Chan where
   (Chan {chan_id = id1}) == (Chan {chan_id = id2}) = id1 == id2
   (DelayChan {chan_rate = r1}) == (DelayChan {chan_rate = r2}) = r1 == r2
   c1 == c2 = False

-- Channels are ordered with the *highest rate* channel
-- being lowest, with the Id as the tie-breaker.  
-- That means that when we sort the channels in increasing
-- order, the highest-rate channels come first

instance Ord Chan where
   (Chan {}) `compare` (DelayChan {}) = LT
   (Chan {chan_approx = x1, chan_id = id1})
	`compare` 
	(Chan {chan_approx = x2, chan_id = id2}) 
	= {- (x2 `compare` x1) `thenCmp` -} (id1 `compare` id2)
   (DelayChan {chan_rate = r1}) `compare` (DelayChan {chan_rate = r2}) 
	= r1 `compare` r2
   (DelayChan {}) `compare` (Chan {}) = GT

instance Show Chan where
  show (Chan {chan_id = n})        = show n
  show (DelayChan {chan_rate = r}) = "delay@" ++ show r

-------------------------------
--	Rate
-------------------------------

type Rate = Double
-- Or we could use (Maybe Double)?

infiniteRate :: Rate
infiniteRate = 1/0

isInfiniteRate :: Rate -> Bool
isInfiniteRate = isInfinite

scaledRate  :: Chan	-- Channel on which interaction is happening
	    -> Int	-- Number of cells
	    -> Int	-- Number of interactions in this cell
	    -> Rate
-- Scale the rate of *binary* interactions by the number of cells
scaledRate (Chan { chan_rate = r })      ncells act = r * fromIntegral (ncells * act)
scaledRate (DelayChan { chan_rate = r }) ncells act = r * fromIntegral act


addRates :: [Rate] -> Rate
-- Add a list of rates, assumed to be in decreasing order
-- So test if the first is infinite, to short-circuit 
-- the process
addRates []  = 0
addRates [r] = r
addRates (r:rs) | isInfiniteRate r = infiniteRate
		| otherwise	   = r + sum rs


-------------------------------
--	Utilities
-------------------------------

thenCmp :: Ordering -> Ordering -> Ordering
thenCmp LT _ = LT
thenCmp EQ x = x
thenCmp GT _ = GT
