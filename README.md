prolog-target-js
================

Simple Prolog to JS transpiler. The project is at rather incomplete state at the moment.

Example input:

```prolog
append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]):- append(Xs, Ys, Zs).
```

Example output:

```javascript
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
```

Calling the predicate
---------------------

```javascript
var runtime = require('../runtime');
var util = require('../util');
var append = require('./append');

var list1 = util.array2List([1, 2, 3, 4, 5]);
var list2 = util.array2List([6, 7, 8, 9, 10]);

var result = new runtime.Var();

runtime.run(append.append_3(list1, list2, result, [], null));

console.log('Result is: ' + runtime.toString(result));
```

