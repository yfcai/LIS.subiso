all:
	ghc -O iso_test.hs -o bin/isubiso && \
	ghc -O tikz.hs -o bin/tikz && \
	rm *.hi *.o

clean:
	rm -f *.hi *.o bin/isubiso bin/tikz