#ghcpath="/media/rmduarte/rmduarte/ghc-8.4.3/inplace/bin"
#rtsopt="--make -O -threaded -fglasgow-exts"

SI: Main.hs
	/mnt/rmduarte/Repositorios/ghc_modify_86/inplace/bin/ghc-stage2 Main.hs --make -O -threaded -debug -fglasgow-exts -rtsopts$
	#/mnt/rmduarte/rmduarte/Repositorios/ghc_modify_86/inplace/bin/ghc-stage2 Main.hs --make -O -threaded -debug -fglasgow-exts -rtsopts -fno-warn-tabs -o SI
	
	#/media/rmduarte/rmduarte/ghc-8.2.2/inplace/bin/ghc-stage2 Main.hs -v --make -O -threaded -fglasgow-exts -rtsopts -o SI
	#ghc Main.hs --make -O -threaded -fglasgow-exts -rtsopts -fno-warn-tabs -o SI

clean:
	rm *.hi
	rm *.o
	rm Main 
