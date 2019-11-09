module Main where

import Control.Concurrent
import GHC.Conc
import Control.Monad
import System.Random as Random
import System.Time
import Text.Printf
import System

import RunSoup( runSoup )
import Soup( emptySoup, showDebugHeaders )
import Compile( compileProgram )
import Parser( parseProgram )
import Cell
import IO
import System.Environment( getArgs )

-- Number of cells (~ number of processors), etc.
-- Make these a constant for now, but really they should
-- be read off the command line.
nCells = 20

exportFraction :: Float
exportFraction = 0.1

main 
  = do	{ args <- getArgs
	; if null args then 
	    hPutStrLn stderr "Usage: hspim-p <file>.spi"
	  else
	    runPgm (head args)
	--; stats <- readTStats
        --; putStrLn (show stats)
	}

runPgm file
  = do	{ input <- openFile file ReadMode
	; cts <- hGetContents input
	; let decls = parseProgram file cts
	; let (samples, plotspec, init_procs) = compileProgram decls 0
	      (tot_time, nlines) = samples
   	      epochLength = tot_time / fromIntegral nlines
	; log_handle <- openFile (file ++ "-hs-p.csv") WriteMode
        -- ; showDebugHeaders 
        ; runCells log_handle samples plotspec init_procs 
		   nCells epochLength exportFraction
	; hClose log_handle
	}

