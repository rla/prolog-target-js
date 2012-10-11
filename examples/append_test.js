var runtime = require('../runtime');
var util = require('../util');
var append = require('./append');

function cons(head, tail) {
	return new Struct('.', head, tail);
}

var list1 = util.array2List([1, 2, 3, 4, 5]);
var list2 = util.array2List([6, 7, 8, 9, 10]);

var result = new runtime.Var();

runtime.run(append.append_3(list1, list2, result, [], null));

console.log('Result is: ' + runtime.toString(result));
