wrangell : datatypes.hs parser.hs main.hs evaluation.hs errors.hs functions.hs
	ghc -o wrangell --make main.hs

clean : 
	rm *.o
	rm *.hi
	rm wrangell
