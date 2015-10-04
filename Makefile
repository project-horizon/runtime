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
	echo ';var mreg=this["$$module_register$$"];for(var v in mreg){console.log(mreg[v].export());}' >> /tmp/dump.js
	node /tmp/dump.js

