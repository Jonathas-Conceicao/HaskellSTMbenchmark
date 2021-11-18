# A fork of the Haskell STM Benchmark

This repository contains the Haskell STM Benchmark updated to run on newer versions of the GHC.

Be sure to check the other branches for variations done on this project.

## About the IOSTM

Some of the examples here have been tweeked to also have a implementation using Transactional Boosting,
over STM Haskell made possible by the IOSTM extension.

The IOSTM implementation can be found on [this fork](https://github.com/Jonathas-Conceicao/ghc/tree/handlers-implementation-8.6)

The current updated examples are:

- [1-SI](1-SI)
- [X-Set](X-Set) an added example using linked list to construct a Set structure

## Original readme from the Haskell STM Benchmark

```
*************************************************************
***** The Haskell STM Benchmark version 0.1		*****
***** as gathered by Nehir Sonmez & Cristian Perfumo 	*****
***** (nehir.sonmez@bsc.es & cristian.perfumo@bsc.es)	*****
***** BSC-Microsoft Research Center (www.bscmsrc.eu)	*****	
***** January 2009					*****
*************************************************************

TO COMPILE AND RUN: Find the two scripts scriptRun and scriptCompile in the 0-scripts/ directory and before running them, please make sure to change the variables that correctly point to the GHC path (ghcpath) and the benchmark path (benchpath). The results can be found in the x-Results/ directory.

The benchmark consists of Haskell Software Transactional Memory (STM) implementations of the following programs:
- Shared Integer-- by C. Perfumo & N. Sonmez
- Linked Lists	-- by C. Perfumo & N. Sonmez (has 3 versions: (i) STM, (ii) STM w/ unreadTVar (see papers below), and (iii) non-STM implementation using IORefs)
- Binary Trees	-- by C. Perfumo & N. Sonmez (has 3 versions, (i) STM, (ii) STM w/ unreadTVar (see papers below), and (iii) non-STM implementation using IORefs)
- Hash Table 	-- by E. Kmett (http://comonad.com/haskell/thash/dist/)
- TCache	-- by A. G. Corona (http://hackage.haskell.org/cgi-bin/hackage-scripts/package/TCache)
- ParallelSudoku-- by W. Swierstra (http://www.haskell.org/sitewiki/images/1/12/SudokuWss.hs)
- CCHR		-- by E. S. L. Lam & M. Sulzmann (http://taichi.ddns.comp.nus.edu.sg/taichiwiki/CCHR)
- SPI Calculus	-- by S. Peyton-Jones (http://research.microsoft.com/en-us/people/simonpj/)

Tested with: 		ghc-6.6.1, ghc-6.8.2
Required packages: 	libghc6-parsec-dev, libghc6-stm-dev

The Haskell STM Benchmark brings together a collection of Haskell programs written using STM that have been used as a benchmark in the following studies:
- N. Sonmez, C. Perfumo, S. Stipic, A. Cristal, O. S. Unsal and M. Valero, "Why you should profile your Transactional Memory applications on an atomic block basis: A Haskell case study" MULTIPROG 2009, Paphos, Cyprus, January 2009.
- C. Perfumo, N. Sonmez, S. Stipic, A. Cristal, O. S. Unsal, T. Harris and M. Valero, “The Limits of Software Transactional Memory (STM): Dissecting Haskell STM Applications on a Many-Core Environment”, in Computing Frontiers 2008, Ischia, Italy, May 5-7, 2008.
- C. Perfumo, N. Sonmez, O. S. Unsal, A. Cristal, M. Valero, and T. Harris, “Dissecting Transactional Executions in Haskell”, TRANSACT 07, Portland, Oregon, USA, August 2007.
- N. Sonmez, C. Perfumo, S. Stipic, A. Cristal, O. S. Unsal and M. Valero, “UnreadTVar: Extending Haskell Software Transactional Memory for Performance”, in Eighth Symposium on Trends in Functional Programming (TFP 2007), New York, USA, April 2007.
```
