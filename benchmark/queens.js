var runtime = require("../runtime");
var Var = runtime.Var;
var Struct = runtime.Struct;
var _u = runtime.unification;
var _b = runtime.backtrack;
var _is = runtime.is;
var _inequal = runtime.inequal;
var _less = runtime.less;
var _cut = runtime.cut;
var _s = runtime.toString;

function range_3_0($0, $1, $2, s, cb) {
    var $3 = new Var();
    var _i = s.length;
    s.push(function() {
        return range_3_1($0, $1, $2, s, cb);
    });
    if (!_u(s, $0, $3)) return _b(s);
    if (!_u(s, $1, $3)) return _b(s);
    if (!_u(s, $2, new Struct('.', $3, '[]'))) return _b(s);
    _cut(s, _i);
    return cb;
}
function range_3_1($0, $1, $2, s, cb) {
    var $3 = new Var();
    var $4 = new Var();
    var $5 = new Var();
    var $6 = new Var();
    var _i = s.length;
    if (!_u(s, $0, $3)) return _b(s);
    if (!_u(s, $1, $4)) return _b(s);
    if (!_u(s, $2, new Struct('.', $3, $5))) return _b(s);
    return _less($3, $4, s, function() {
        return _is($6, new Struct('+', $3, 1), s, function() {
            return range_3_0($6, $4, $5, s, cb);
        });
    });
}
exports.range_3 = range_3_0;

function select_3_0($0, $1, $2, s, cb) {
    var $3 = new Var();
    var $4 = new Var();
    var _i = s.length;
    s.push(function() {
        return select_3_1($0, $1, $2, s, cb);
    });
    if (!_u(s, $0, new Struct('.', $3, $4))) return _b(s);
    if (!_u(s, $1, $4)) return _b(s);
    if (!_u(s, $2, $3)) return _b(s);
    return cb;
}
function select_3_1($0, $1, $2, s, cb) {
    var $3 = new Var();
    var $4 = new Var();
    var $5 = new Var();
    var $6 = new Var();
    var _i = s.length;
    if (!_u(s, $0, new Struct('.', $3, $4))) return _b(s);
    if (!_u(s, $1, new Struct('.', $3, $5))) return _b(s);
    if (!_u(s, $2, $6)) return _b(s);
    return select_3_0($4, $5, $6, s, cb);
}
exports.select_3 = select_3_0;

function not_attack_3_0($0, $1, $2, s, cb) {
    var $3 = new Var();
    var $4 = new Var();
    var _i = s.length;
    s.push(function() {
        return not_attack_3_1($0, $1, $2, s, cb);
    });
    if (!_u(s, $0, '[]')) return _b(s);
    if (!_u(s, $1, $3)) return _b(s);
    if (!_u(s, $2, $4)) return _b(s);
    _cut(s, _i);
    return cb;
}
function not_attack_3_1($0, $1, $2, s, cb) {
    var $3 = new Var();
    var $4 = new Var();
    var $5 = new Var();
    var $6 = new Var();
    var $7 = new Var();
    var _i = s.length;
    if (!_u(s, $0, new Struct('.', $3, $4))) return _b(s);
    if (!_u(s, $1, $5)) return _b(s);
    if (!_u(s, $2, $6)) return _b(s);
    return _inequal($5, new Struct('+', $3, $6), s, function() {
        return _inequal($5, new Struct('-', $3, $6), s, function() {
            return _is($7, new Struct('+', $6, 1), s, function() {
                return not_attack_3_0($4, $5, $7, s, cb);
            });
        });
    });
}
exports.not_attack_3 = not_attack_3_0;

function not_attack_2_0($0, $1, s, cb) {
    var $2 = new Var();
    var $3 = new Var();
    var _i = s.length;
    if (!_u(s, $0, $2)) return _b(s);
    if (!_u(s, $1, $3)) return _b(s);
    return not_attack_3_0($2, $3, 1, s, cb);
}
exports.not_attack_2 = not_attack_2_0;

function queens_3_0($0, $1, $2, s, cb) {
    var $3 = new Var();
    var _i = s.length;
    s.push(function() {
        return queens_3_1($0, $1, $2, s, cb);
    });
    if (!_u(s, $0, '[]')) return _b(s);
    if (!_u(s, $1, $3)) return _b(s);
    if (!_u(s, $2, $3)) return _b(s);
    return cb;
}
function queens_3_1($0, $1, $2, s, cb) {
    var $3 = new Var();
    var $4 = new Var();
    var $5 = new Var();
    var $6 = new Var();
    var $7 = new Var();
    var _i = s.length;
    if (!_u(s, $0, $3)) return _b(s);
    if (!_u(s, $1, $4)) return _b(s);
    if (!_u(s, $2, $5)) return _b(s);
    return select_3_0($3, $6, $7, s, function() {
        return not_attack_2_0($4, $7, s, function() {
            return queens_3_0($6, new Struct('.', $7, $4), $5, s, cb);
        });
    });
}
exports.queens_3 = queens_3_0;

function queens_2_0($0, $1, s, cb) {
    var $2 = new Var();
    var $3 = new Var();
    var $4 = new Var();
    var _i = s.length;
    if (!_u(s, $0, $2)) return _b(s);
    if (!_u(s, $1, $3)) return _b(s);
    return range_3_0(1, $2, $4, s, function() {
        return queens_3_0($4, '[]', $3, s, cb);
    });
}
exports.queens_2 = queens_2_0;
