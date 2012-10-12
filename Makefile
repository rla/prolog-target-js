benchmark/naiverev.js: benchmark/naiverev.pl
	swipl -f src/compiler.pl -t transpile $<

naiverev: benchmark/naiverev.js
	node benchmark/naiverev_benchmark.js
	
profile: benchmark/naiverev.js
	node --prof benchmark/naiverev_benchmark.js
	node-tick-processor
	
beautify: benchmark/naiverev.js
	js-beautify -o $<.beautified.js $<
	
examples/append.js: examples/append.pl
	swipl -f src/compiler.pl -t transpile $<	
	
append: examples/append.js
	js-beautify -o $<.beautified.js $<
	
clean:
	rm -f benchmark/naiverev.js
	rm -f benchmark/naiverev.js.beautified.js
	rm -f examples/append.js
	rm -f examples/append.js.js.beautified.js
	rm -f v8.log
	
check:
	jshint runtime.js

.PHONY: clean check