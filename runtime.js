var util = require('util');

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
}

exports.Var = Var;
exports.Struct = Struct;

// Dereferences given term.

function deref(term) {
	while (true) {
		if (term instanceof Var) {
			if (term == term.ref) {
				return term;
			} else {
				term = term.ref;
			}
		} else {
			return term;
		}
	}
}

// Top-level unification to use in predicates.

exports.unify = function(a, b, stack, cb) {
	if (unification(stack, a, b)) {
		return cb;
	} else {
		return backtrack(stack);
	}
}

// Runs the goal.

exports.run = function(cb) {
	while (cb = cb());
}

// Runs backtracking.
// Undoes variables, returns next goal.

function backtrack(stack) {
	var top;
	while (top = stack.pop()) {
		if (top instanceof Var) {
			top.ref = top;
		} else {
			return top;
		}
	}
	throw new Error('No more choices');
}

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
	
	console.log(toString(ad) + ' ' + toString(bd));
	
	if (ad instanceof Var) {
		ad.ref = bd;
		stack.push(ad);
	} else if (bd instanceof Var) {
		bd.ref = ad;
		stack.push(bd);
	} else if (ad instanceof Struct) {
		if (bd instanceof Struct) {
			if (ad.arguments[0] == bd.arguments[0]
				&& ad.arguments.length == bd.arguments.length) {				
				for (var i = ad.arguments.length - 1; i >= 1; i--) {
					if (!unification(stack, ad.arguments[i], bd.arguments[i])) {
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
		return ad == bd;
	}
	
	return true;
}

