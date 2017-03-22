wrangell : *.hs
	ghc -o wrangell --make main.hs 

clean : 
	rm *.o
	rm *.hi
	rm wrangell
