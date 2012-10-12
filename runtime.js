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

// Top-level unification to use in predicates.

exports.unify = function(a, b, s, cb) {
	if (unification(s, a, b)) {
		return cb;
	} else {
		return backtrack(s);
	}
}

// Runs the goal.

exports.run = function(cb) {
	while (cb = cb());
};

// Runs backtracking.
// Undoes variables, returns next goal.

function backtrack(stack) {
	var top;
	while (top = stack.pop()) {
		if (typeof top === 'function') {
			return top;
		} else {
			top.ref = top;
		}
	}
	throw new Error('No more choices');
}

exports.backtrack = backtrack;

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

// Optimized version of unification.
// Actually slower than recursive version.

function unification_stack(s, a, b) {
	var ulstack = [a];
	var urstack = [b];
	
	while (ulstack.length > 0) {
		a = ulstack.pop();
		b = urstack.pop();
		while (a instanceof Var) {			
			if (a === a.ref) {
				break;
			}
			a = a.ref;
		}
		while (b instanceof Var) {			
			if (b === b.ref) {
				break;
			}
			b = b.ref;
		}
		if (a === b) {
			continue;
		}
		if (a instanceof Var) {
			a.ref = b;
			s.push(a);
			continue;
		}
		if (b instanceof Var) {
			b.ref = a;
			s.push(b);
			continue;
		}
		if (a instanceof Struct) {
			if (b instanceof Struct) {
				var as = a.arguments;
				var bs = b.arguments;
				if (as.length !== bs.length || as[0] !== bs[0]) {
					return false;
				}
				var n = as.length;
				for (var i = 1; i < n; i++) {
					ulstack.push(as[i]);
					urstack.push(bs[i]);
				}
				continue;
			} else {
				return false;
			}
		}
		return false;
	}
	return true;
}

exports.unification = unification;

