let%test _ = Hw13.interp_prog (ParserMain.parse "x = 3; y = 0; while x > 0 { y = y + x; x = x - 1;}") = [("x", Value.NumV 0); ("y", Value.NumV 6)]
let%test _ = Hw13.interp_prog (ParserMain.parse "x = 3; y = 7; z = x < y; if z { z = y - x;}") = [("z", Value.NumV 4); ("y", Value.NumV 7); ("x", Value.NumV 3)]
let%test _ = Hw13.interp_prog (ParserMain.parse "x = 3; y = 7; z = x < y; if x > y {y = x - 4;} if y == -1 { y = x - 4;} else { z = x + y; z = z - 5;}") = [("z", Value.NumV 5); ("y", Value.NumV 7); ("x", Value.NumV 3)]

(* additional test 
let%test _ = Hw13.interp_prog (ParserMain.parse "x = 3; y = 10;") = [("y", Value.NumV 10); ("x", Value.NumV 3)]
let%test _ = Hw13.interp_prog (ParserMain.parse "x = 3; y = true; z = true == 3;") = [("z", Value.BoolV false); ("y", Value.BoolV true); ("x", Value.NumV 3)]
let%test _ = Hw13.interp_prog (ParserMain.parse "x = false; y = true; z = false || true;") = [("z", Value.BoolV true); ("y", Value.BoolV true); ("x", Value.BoolV false)]
let%test _ = Hw13.interp_prog (ParserMain.parse "x = false; y = true; z = false && true;") = [("z", Value.BoolV false); ("y", Value.BoolV true); ("x", Value.BoolV false)]
let%test _ = Hw13.interp_prog (ParserMain.parse "x = true; y = 0; while x { y = y + 1; if y == 10 {x = false;}}") = [("x", Value.BoolV false); ("y", Value.NumV 10)]

let%test _ = try Hw13.interp_prog (ParserMain.parse "x = 3; y = true; if y {x = x + y;}") = [] with Failure msg -> msg = "[Error] Not a Number: x + y"
let%test _ = try Hw13.interp_prog (ParserMain.parse "x = 3; y = true; if y {x = x - y;}") = [] with Failure msg -> msg = "[Error] Not a Number: x - y"
let%test _ = try Hw13.interp_prog (ParserMain.parse "x = 3; y = true; if x > y {x = 4;}") = [] with Failure msg -> msg = "[Error] Not a Number: x > y"
let%test _ = try Hw13.interp_prog (ParserMain.parse "x = 3; y = true; if x < y {x = 4;}") = [] with Failure msg -> msg = "[Error] Not a Number: x < y"
let%test _ = try Hw13.interp_prog (ParserMain.parse "x = 3; y = true; if x && y {x = 4;}") = [] with Failure msg -> msg = "[Error] Not a Bool: x && y"
let%test _ = try Hw13.interp_prog (ParserMain.parse "x = 3; y = true; if x || y {x = 4;}") = [] with Failure msg -> msg = "[Error] Not a Bool: x || y"
let%test _ = try Hw13.interp_prog (ParserMain.parse "x = 3; y = true; if x {x = 4;}") = [] with Failure msg -> msg = "[Error] Not a Bool: x"
*)