CC=ghc
flags=-O2

default: 
	$(CC) $(flags) Data/Image.hs

clean:
	rm Data/*.o Data/*.hi rm Data/*.hs~ Data/Image/*.o Data/Image/*.hi rm Data/Image/*.hs~