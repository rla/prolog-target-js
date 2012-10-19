all: compile

SRC = benchmark/naiverev.pl benchmark/queens.pl examples/append.pl
JS = benchmark/naiverev.js benchmark/queens.js examples/append.js

%.js: %.pl
	swipl -q -f src/compiler.pl -t transpile $<
	js-beautify -r $@

compile: $(JS)
	
profile: benchmark/naiverev.js
	node --prof benchmark/naiverev_benchmark.js
	node-tick-processor
	
clean:
	rm -f $(JS)
	rm -f v8.log
	
check:
	jshint runtime.js

.PHONY: clean check