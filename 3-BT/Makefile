#ghcpath="/media/rmduarte/rmduarte/ghc-8.4.3/inplace/bin"
#rtsopt="--make -O -threaded -fglasgow-exts"

BT: Main.hs
	/media/rmduarte/rmduarte/Repositorios/ghc_modify/inplace/bin/ghc-stage2 Main.hs -v --make -O -threaded -fglasgow-exts -fno-warn-tabs -o BT
	#/media/rmduarte/rmduarte/ghc-8.2.2/inplace/bin/ghc-stage2 Main.hs -v --make -O -threaded -fglasgow-exts -o BT
	#ghc Main.hs --make -O -threaded -fglasgow-exts -o BT

clean:
	rm *.hi
	rm *.o
	rm BT  
