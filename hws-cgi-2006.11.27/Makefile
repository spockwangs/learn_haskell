GHC = ghc
GHCFLAGS = -isrc -cpp -threaded -fwarn-unused-imports 

.PHONY: hws debug

hws:
	$(GHC) $(GHCFLAGS) --make -o $@ src/Main.hs

dynhs: GHCFLAGS += -DDYNHS -package ghc
dynhs: hws

opt: GHCFLAGS += -O
opt: hws

clean: 
	find src '(' -name '*.o' -o -name '*.hi' ')' -exec rm -f '{}' ';'
	-rm -f hws
