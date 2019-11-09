#ghcpath="/media/rmduarte/rmduarte/ghc-8.4.3/inplace/bin"
#rtsopt="--make -O -threaded -fglasgow-exts"

HT: Main.hs
	/media/rmduarte/rmduarte/Repositorios/ghc_modify/inplace/bin/ghc-stage2 Main.hs -v --make -O -threaded -fglasgow-exts -rtsopts -o HT
	#/media/rmduarte/rmduarte/ghc-8.2.2/inplace/bin/ghc-stage2 Main.hs -v --make -O -threaded -fglasgow-exts -rtsopts -o HT
	#ghc Main.hs --make -O -threaded -fglasgow-exts -rtsopts -o HT

clean:
	rm *.hi
	rm *.o
	rm Data/*.hi
	rm Data/*.o
	rm Data/THash/*.hi
	rm Data/THash/*.o
	rm HT 
