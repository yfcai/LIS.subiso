all:
	ghc -O iso_test.hs -o graphs/isubiso && \
	ghc -O tikz.hs -o graphs/tikz && \
	rm *.hi *.o

clean:
	rm -f *.hi *.o graphs/isubiso graphs/tikz