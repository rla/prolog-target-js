var util = require('../util');
var runtime = require('../runtime');
var naiverev = require('./naiverev');
var Var = runtime.Var;

var array = [];
for (var i = 0; i < 100; i++) {
	array.push(i);
}

var list = util.array2List(array);
var reverse = new Var();

var top = naiverev.naiverev_2(list, reverse, [], null);

console.time('reverse');
runtime.run(top);
console.timeEnd('reverse');

console.log('Reversed is: ' + runtime.toString(reverse));
