var util = require('../util');
var runtime = require('../runtime');
var queens = require('./queens');
var Var = runtime.Var;

var stack = [];
var placement = new Var();
var top = queens.queens_2(11, placement, stack, null);

console.time('queens');
runtime.run(top);

//console.log('Placement is: ' + runtime.toString(placement));

while (runtime.retry(stack)) {
	//console.log('Another solution: ' + runtime.toString(placement));
}

console.timeEnd('queens');