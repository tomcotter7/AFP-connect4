all:
	ghc --make -O2 -dynamic connect4.lhs

clean:
	rm -f *.o *.hi connect4
