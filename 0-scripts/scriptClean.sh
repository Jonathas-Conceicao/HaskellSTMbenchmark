#!/bin/bash

cd ..
benchpath=`pwd`
echo benchpath

#clean .o's,  prepare directories

echo SI
cd $benchpath/1-SI/
              rm *.o
              rm *.hi
              rm Main

echo LL
cd $benchpath/2-LL/
	      find . -name '*.o' -exec rm {} \;
        find . -name '*.hi' -exec rm {} \;
        rm Main

echo BT
cd $benchpath/3-BT/
	      find . -name '*.o' -exec rm {} \;
        find . -name '*.hi' -exec rm {} \;
        rm Main

echo HT
cd $benchpath/4-HT/
	      find . -name '*.o' -exec rm {} \;
        find . -name '*.hi' -exec rm {} \;
        rm Main
# cd $benchpath/5-TCache/
# 	      rm Main
# 	      find . -name '*.o' -exec rm {} \;
# 	      find . -name '*.hi' -exec rm {} \;

echo ParallelSudoku
cd $benchpath/6-ParallelSudoku/
              rm *.o
              rm *.hi
              rm Main

echo CCHR
cd $benchpath/7-CCHR/
              find . -name '*.o' -exec rm {} \;
              find . -name '*.hi' -exec rm {} \;
              rm Blockworld
              rm Gcd
              rm Prime
              rm Sudoku
              rm Unionfind

echo SPI
cd $benchpath/8-spi/
	      find . -name '*.o' -exec rm {} \;
        find . -name '*.hi' -exec rm {} \;
        rm parallel/Main
