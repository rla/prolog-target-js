function naiverev_2($0, $1, s, cb) {
	function naiverev_2_0($0, $1, s, cb) {
		s.push(function() {
			return naiverev_2_1($0, $1, s, cb);
		});
		return unify_2($0, '[]', s, function() {
			return unify_2($1, '[]', s, function() {
				return cb;
			});
		});
	}
	function naiverev_2_1($0, $1, s, cb) {
		var $2 = new Var(), $3 = new Var(), $4 = new Var(), $5 = new Var();
		return unify_2($0, new Struct('.', $2, $3), s, function() {
			return unify_2($1, $4, s, function() {
				return naiverev_2($3, $5, s, function() {
					return append_3($5, new Struct('.', $2, '[]'), $4, s,
							function() {
								return cb;
							});
				});
			});
		});
	}
	return naiverev_2_0($0, $1, s, cb);
}
exports.naiverev_2 = naiverev_2;
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
exports.append_3 = append_3;
