prolog-target-js
================

Simple Prolog to JS transpiler. The project is at very incomplete state at the moment.

Example input:

```prolog
append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]):- append(Xs, Ys, Zs).
```

Example output:

```javascript
function append_3($0, $1, $2, s, cb) {
	function append_3_0($0, $1, $2, s, cb) {
		var $3 = new Var();
		s.push(function() {
			return append_3_1($0, $1, $2, s, cb);
		});
		return unify_2($0, '[]', s, function() {
			return unify_2($1, $3, s, function() {
				return unify_2($2, $3, s, function() {
					return cb;
				});
			});
		});
	}
	function append_3_1($0, $1, $2, s, cb) {
		var $3 = new Var(), $4 = new Var(), $5 = new Var(), $6 = new Var();
		return unify_2($0, new Struct('.', $3, $4), s, function() {
			return unify_2($1, $5, s, function() {
				return unify_2($2, new Struct('.', $3, $6), s, function() {
					return append_3($4, $5, $6, s, function() {
						return cb;
					});
				});
			});
		});
	}
	return append_3_0($0, $1, $2, s, cb);
}
```
