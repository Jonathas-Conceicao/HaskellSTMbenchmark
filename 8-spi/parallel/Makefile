all:
	ghc --make Main.hs -o hspim-p

opt:
	ghc --make -O Main.hs -o hspim-p

tar: clean
	(cd ..; tar cfz parallel.tgz parallel)

test: all
	./hspim-p test.spi
	@echo
	@echo ----------------------------
	@cat test.spi-hs-p.csv

debug:
	ghc --make Main.hs -o hspim-p -debug
	./hspim-p test.spi +RTS -Ds

clean:
	rm -rf *~ *.hi *.o *.csv a.out hspim hspim-p *.exe

