sandbox:
	cabal sandbox init

install:
	cabal install ./framework ./runtime

run:
	./.cabal-sandbox/bin/nessy-runtime

