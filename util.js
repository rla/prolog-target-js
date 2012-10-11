var runtime = require('./runtime');
var Struct = runtime.Struct;

exports.array2List = function(array) {
	var tail = '[]';
	for (var i = array.length - 1; i >= 0; i--) {
		tail = new Struct('.', array[i], tail);
	}
	return tail;
}