all:
	ghc -O iso_test.hs -o graphs/isubiso && \
	rm *.hi *.o

clean:
	rm -f *.hi *.o graphs/isubiso