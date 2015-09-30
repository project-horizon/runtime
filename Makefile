sandbox:
	cabal sandbox init

install:
	cabal install ./framework ./runtime

run:
	make install
	./.cabal-sandbox/bin/nessy-runtime

exec:
	make install
	./.cabal-sandbox/bin/nessy-runtime | node

dump:
	make install
	./.cabal-sandbox/bin/nessy-runtime > /tmp/dump.js
	echo ';console.log(this["$$module_register$$"]);' >> /tmp/dump.js
	node /tmp/dump.js
	rm -f /tmp/dump.js

