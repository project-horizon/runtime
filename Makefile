sandbox:
	cabal sandbox init

install:
	cabal install ./framework ./runtime

run:
	make install && ./.cabal-sandbox/bin/nessy-runtime

exec:
	make install && ./.cabal-sandbox/bin/nessy-runtime | node

