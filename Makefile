benchmark/naiverev.js: benchmark/naiverev.pl
	./transpile.sh $<
	cat benchmark/header.js >> $@

naiverev: benchmark/naiverev.js
	node $<
