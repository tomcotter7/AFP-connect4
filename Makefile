all:
	ghc -O2 connect4.lhs

clean:
	rm -f *.o *.hi connect4
