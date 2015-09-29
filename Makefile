sandbox:
	cabal sandbox init
all:	install run

install:
	cabal install ./framework ./runtime

run:
	./.cabal-sandbox/bin/nessy-runtime


