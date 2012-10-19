var runtime = require("../runtime");
var Var = runtime.Var;
var Struct = runtime.Struct;
var _u = runtime.unification;
var _b = runtime.backtrack;
var _is = runtime.is;
var _inequal = runtime.inequal;
var _less = runtime.less;
var _cut = runtime.cut;

function naiverev_2_0($0, $1, s, cb) {
    var _i = s.length;
    s.push(function() {
        return naiverev_2_1($0, $1, s, cb);
    });
    if (!_u(s, $0, '[]')) return _b(s);
    if (!_u(s, $1, '[]')) return _b(s);
    return cb;
}
function naiverev_2_1($0, $1, s, cb) {
    var $2 = new Var();
    var $3 = new Var();
    var $4 = new Var();
    var $5 = new Var();
    var _i = s.length;
    if (!_u(s, $0, new Struct('.', $2, $3))) return _b(s);
    if (!_u(s, $1, $4)) return _b(s);
    return naiverev_2_0($3, $5, s, function() {
        return append_3_0($5, new Struct('.', $2, '[]'), $4, s, cb);
    });
}
exports.naiverev_2 = naiverev_2_0;

function append_3_0($0, $1, $2, s, cb) {
    var $3 = new Var();
    var _i = s.length;
    s.push(function() {
        return append_3_1($0, $1, $2, s, cb);
    });
    if (!_u(s, $0, '[]')) return _b(s);
    if (!_u(s, $1, $3)) return _b(s);
    if (!_u(s, $2, $3)) return _b(s);
    return cb;
}
function append_3_1($0, $1, $2, s, cb) {
    var $3 = new Var();
    var $4 = new Var();
    var $5 = new Var();
    var $6 = new Var();
    var _i = s.length;
    if (!_u(s, $0, new Struct('.', $3, $4))) return _b(s);
    if (!_u(s, $1, $5)) return _b(s);
    if (!_u(s, $2, new Struct('.', $3, $6))) return _b(s);
    return append_3_0($4, $5, $6, s, cb);
}
exports.append_3 = append_3_0;
