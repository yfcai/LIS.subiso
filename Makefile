all:
	ghc -O iso_test.hs -o bin/isub && \
	ghc -O tikz.hs -o bin/tikz && \
	rm *.hi *.o

clean:
	rm -f *.hi *.o bin/isub bin/tikz
