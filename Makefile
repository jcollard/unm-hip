CC=ghc
flags=-O2

default: 
	$(CC) $(flags) Data/Image.hs

haddock: Data/*.hs Data/Image/*.hs
	rm haddock -rf
	mkdir haddock
	haddock -h -o haddock/ Data/*.hs Data/Image/*.hs

clean:
	rm Data/*.o Data/*.hi Data/Image/*.o Data/Image/*.hi