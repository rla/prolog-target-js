var runtime = require('../runtime');

var Var = runtime.Var;
var Struct = runtime.Struct;
var unify = runtime.unify;

function cons(head, tail) {
	return new Struct('.', head, tail);
}

var list = cons(1, cons(2, cons(3, cons(4, '[]'))));
var prefix = cons(1, cons(2, '[]'));
var suffix = new Var();

var top = append_3(prefix, suffix, list, [], null);

runtime.run(top);

console.log('Suffix is: ' + runtime.toString(suffix));

function append_3($0, $1, $2, stack, cb) {
	function append_3_0($0, $1, $2, stack, cb) {
		var $3 = new Var();
		stack.push(function() {
			return append_3_1($0, $1, $2, stack, cb);
		});
		return unify($0, '[]', stack, function() {
			return unify($1, $3, stack, function() {
				return unify($2, $3, stack, function() {
					return cb;
				});
			});
		});
	}
	function append_3_1($0, $1, $2, stack, cb) {
		var $3 = new Var(), $4 = new Var(), $5 = new Var(), $6 = new Var();
		return unify($0, new Struct('.', $3, $4), stack, function() {
			return unify($1, $5, stack, function() {
				return unify($2, new Struct('.', $3, $6), stack, function() {
					return append_3($4, $5, $6, stack, function() {
						return cb;
					});
				});
			});
		});
	}
	return append_3_0($0, $1, $2, stack, cb);
}