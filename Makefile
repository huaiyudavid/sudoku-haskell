TARGETS=hello

%: %.hs
	ghc -o $@ --make $<

all: main

main: $(TARGETS)

clean:
	rm -fr $(TARGETS) $(foreach ext, o hi, $(foreach file, $(TARGETS), $(file).$(ext)))
