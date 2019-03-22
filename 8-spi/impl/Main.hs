module Main where

import RunSoup( runSoup, initM )
import Soup( emptySoup )
import Compile( compileProgram )
import Parser(	parseProgram )
import IO
import System.Environment( getArgs )
import GHC.Conc

main 
  = do	{ args <- getArgs
	; if null args then 
	    hPutStrLn stderr "Usage: hspim <file>.spi"
	  else
	    runPgm (head args)
	}


runPgm file
  = do	{ input <- openFile file ReadMode
	; cts <- hGetContents input
	; let decls = parseProgram file cts
	; let (prog_info, uniq) = compileProgram decls
	      (samples, plots, init_procs) = prog_info

	; log_handle <- openFile (file ++ "-hs.csv") WriteMode
	; let soup = emptySoup log_handle samples plots
	; initM uniq (runSoup soup init_procs)
	; hClose log_handle
	; stats <- readTStats
        ; putStrLn (show stats)
	}

