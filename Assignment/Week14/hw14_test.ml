let%test _ = Hw14.interp_prog (ParserMain.parse "fun foo() {return 99;} def x; x = foo();") = ([("x", 0)], [(0, Value.NumV 99); ((-1), Value.NumV 99)])
let%test _ = Hw14.interp_prog (ParserMain.parse "fun foo(x) {*x = 99; return 1;} def x; def y; y = foo(&x);") = ([("y", 1); ("x", 0)], [(1, Value.NumV 1); (-1, Value.NumV 1); (0, Value.NumV 99); (2, Value.AddrV 0)])
let%test _ = Hw14.interp_prog (ParserMain.parse "fun bar(x) {def y; y = *x; *y = 100; return 0;} fun foo(x) {def y; *x = 99; y = bar(&x); return 1;} def x; def y; y = foo(&x); ") = ([("y", 1); ("x", 0)], [(1, Value.NumV 1); (-1, Value.NumV 1); (3, Value.NumV 0); (0, Value.NumV 100); (5, Value.AddrV 0); (4, Value.AddrV 2); (2, Value.AddrV 0)])


(* additional test
let%test _ = try Hw14.interp_prog (ParserMain.parse "def x; def y; x = 3; y = true; if y {x = x + y;}") = ([],[]) with Failure msg -> msg = "[Error] Not a Number: x + y"
let%test _ = try Hw14.interp_prog (ParserMain.parse "def x; def y; x = 3; y = true; if y {x = x - y;}") = ([],[]) with Failure msg -> msg = "[Error] Not a Number: x - y"
let%test _ = try Hw14.interp_prog (ParserMain.parse "def x; def y; x = 3; y = true; if x > y {x = 4;}") = ([],[]) with Failure msg -> msg = "[Error] Not a Number: x > y"
let%test _ = try Hw14.interp_prog (ParserMain.parse "def x; def y; x = 3; y = true; if x < y {x = 4;}") = ([],[]) with Failure msg -> msg = "[Error] Not a Number: x < y"
let%test _ = try Hw14.interp_prog (ParserMain.parse "def x; def y; x = 3; y = true; if x && y {x = 4;}") = ([],[]) with Failure msg -> msg = "[Error] Not a Bool: x && y"
let%test _ = try Hw14.interp_prog (ParserMain.parse "def x; def y; x = 3; y = true; if x || y {x = 4;}") = ([],[]) with Failure msg -> msg = "[Error] Not a Bool: x || y"

let%test _ = try Hw14.interp_prog (ParserMain.parse "def x; def y; y = true; x = *y;") = ([],[]) with Failure msg -> msg = "[Error] Not an Address: y"
let%test _ = try Hw14.interp_prog (ParserMain.parse "def x; def y; x = 4; y = true; *x = y;") = ([],[]) with Failure msg -> msg = "[Error] Not an Address: x"

let%test _ = try Hw14.interp_prog (ParserMain.parse "def x; def y; x = 3; y = true; if x {x = 4;}") = ([],[]) with Failure msg -> msg = "[Error] Not a Bool: x"
let%test _ = try Hw14.interp_prog (ParserMain.parse "def x; def y; x = true; y = 3; while y {x = 4;}") = ([],[]) with Failure msg -> msg = "[Error] Not a Bool: y"

let%test _ = try Hw14.interp_prog (ParserMain.parse "fun foo() {return 99;} def x; x = foo(0);") = ([],[]) with Failure msg -> msg = "[Error] arguments mismatch"

let%test _ = try Hw14.interp_prog (ParserMain.parse "def x; y = 1;") = ([],[]) with Failure msg -> msg = "[Error] Free identifier: y"
let%test _ = try Hw14.interp_prog (ParserMain.parse "def x; def y; y = &x; x = *y;") = ([],[]) with Failure msg -> msg = "[Error] Free address: 0"

let%test _ = try Hw14.interp_prog (ParserMain.parse "def x; x = foo();") = ([],[]) with Failure msg -> msg = "[Error] Unbound function: foo"


Hw14.interp_prog (ParserMain.parse "def x; def y; x = 1; y = &x;");;
Hw14.interp_prog (ParserMain.parse "fun foo(x) {*x = 99; return 1;} def x; def y;");;

Hw14.interp_prog (ParserMain.parse "fun foo(x) {*x = 99; return 1;} def x; def y; y = foo(&x);");;
Hw14.interp_prog (ParserMain.parse "fun foo(x) {def y; y = *x; *y = 100; return 0;} def x; def y; y = foo(&x);");;
Hw14.interp_prog (ParserMain.parse "fun bar(x) {def y; y = *x; *y = 100; return 0;} fun foo(x) {def y; *x = 99; y = bar(&x); return 1;} def x; def y; y = foo(&x); ") = ([("y", 1); ("x", 0)], [(1, Value.NumV 1); (-1, Value.NumV 1); (3, Value.NumV 0); (0, Value.NumV 100); (5, Value.AddrV 0); (4, Value.AddrV 2); (2, Value.AddrV 0)])

Hw14.interp_prog (ParserMain.parse "fun fib(x) { if x == 0 {return 0;} else {if x == 1 { return 1;} else { def t1; def t2; t1 = fib(x - 1); t2 = fib(x - 2); return t1 + t2;}}} def x; x = fib(10);")
*)