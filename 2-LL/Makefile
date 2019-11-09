#ghcpath="/media/rmduarte/rmduarte/ghc-8.4.3/inplace/bin"
#rtsopt="--make -O -threaded -fglasgow-exts"

LL: Main.hs
	/media/rmduarte/rmduarte/Repositorios/ghc_modify/inplace/bin/ghc-stage2 Main.hs -v --make -O -threaded -fglasgow-exts -rtsopts -fno-warn-tabs -o LL
	#/media/rmduarte/rmduarte/ghc-8.2.2/inplace/bin/ghc-stage2 Main.hs -v --make -O -threaded -fglasgow-exts -rtsopts -o LL
	#ghc Main.hs --make -O -threaded -fglasgow-exts -rtsopts -fno-warn-tabs -o LL

clean:
	rm *.hi
	rm *.o
	rm LL
