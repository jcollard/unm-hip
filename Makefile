HC=ghc
HCFLAGS=-O2

hsrc = Data/*.hs Data/Image/*.hs

default: 
	$(HC) $(HCFLAGS) Data/Image.hs

haddock: $(hsrc)
	-rm -rf haddock
	haddock -h -o haddock/ $(hsrc)

clean:
	-rm -f $(hsrc:.hs=.o) $(hsrc:.hs=.hi)
	-rm -rf haddock

.PHONY: default clean
