// Constructor for variables.

function Var() {
	this.ref = this;
}

// Constructor for compound terms.

function Struct() {
	this.arguments = arguments;
}

Struct.prototype.toString = function() {
	var args = Array.prototype.slice.call(this.arguments, 1);
	return this.arguments[0] + '(' + args.map(toString).join(', ') + ')';
};

exports.Var = Var;
exports.Struct = Struct;

// Dereferences given term.

function deref(term) {
	while (term instanceof Var) {
		if (term === term.ref) {
			return term;
		} else {
			term = term.ref;
		}
	}
	return term;
}

// Runs the goal.

function run(cb) {
	try {
		while (cb = cb()) {};
		return true;
	} catch (e) {
		return false;
	}
}

exports.run = run;

// Retries the goal.

exports.retry = function(s) {
	var cb = backtrack(s);
	if (cb) {
		return run(cb);
	} else {
		return false;
	}
};

// Runs backtracking.
// Undoes variables, returns next goal.
// Ignores cut choices.

function backtrack(s) {
	var top;
	while (top = s.pop()) {
		if (typeof top === 'function') {
			if (top.cut) {
				continue;
			} else {
				return top;
			}			
		} else {
			top.ref = top;
		}
	}
	throw new Error('No more choices');
}

exports.backtrack = backtrack;

// Marks all choices cut down
// to the given index in the stack.

function cut(s, index) {
	var current = s.length;
	while (current > index) {
		var e = s[current];
		if (typeof e === 'function') {
			e.cut = true;
		}
		current--;
	}
}

exports.cut = cut;

function toString(term) {
	var term = deref(term);
	if (term instanceof Var) {
		return '_';
	} else {
		return term.toString();
	}
}

exports.toString = toString;

// Low-level unification. Does not undo bindings
// itself.

function unification(stack, a, b) {
	var ad = deref(a);
	var bd = deref(b);
	
	if (ad instanceof Var) {
		ad.ref = bd;
		stack.push(ad);
	} else if (bd instanceof Var) {
		bd.ref = ad;
		stack.push(bd);
	} else if (ad instanceof Struct) {
		if (bd instanceof Struct) {
			var aas = ad.arguments;
			var bas = bd.arguments;
			if (aas[0] === bas[0] && aas.length === bas.length) {				
				for (var i = aas.length - 1; i >= 1; i--) {
					if (!unification(stack, aas[i], bas[i])) {
						return false;
					}
				}
			} else {
				return false;
			}
		} else {
			return false;
		}
	} else {
		return ad === bd;
	}
	
	return true;
}

// Helper to evaluate arithmetical expressions.

function calc(exp) {
	var exp = deref(exp);
	
	if (typeof exp === 'number') {
		return exp;
	} else if (exp instanceof Struct) {
		var op = exp.arguments[0];
		var left = calc(exp.arguments[1]);
		var right = calc(exp.arguments[2]);
		if (op === '+') {
			return left + right;
		} else if (op === '-') {
			return left - right;
		} else if (op === '*') {
			return left * right;
		} else if (op === '/') {
			return left / right;
		} else {
			throw new Error('Unknown artithmetical operator ' + op);
		}
	} else {
		throw new Error('Invalid arithmetical expression ' + exp);
	}
}

// Implements is/2 predicate.

function is(v, expression, s, cb) {
	var v = deref(v);
	
	if (!(v instanceof Var)) {
		throw new Error('Left side of is/2 is not a variable');
	}
	
	v.ref = calc(expression);
	s.push(v);
	
	return cb;
}

// Implements =\=/2 predicate.

function inequal(left, right, s, cb) {
	if (calc(left) !== calc(right)) {
		return cb;
	} else {
		return backtrack(s);
	}
}

// Implements </2 predicate.

function less(left, right, s, cb) {
	if (deref(left) < deref(right)) {
		return cb;
	} else {
		return backtrack(s);
	}
}

exports.unification = unification;
exports.is = is;
exports.inequal = inequal;
exports.less = less;