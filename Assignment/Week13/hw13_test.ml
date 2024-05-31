let%test _ = Hw13.interp_prog (ParserMain.parse "x = 3; y = 0; while x > 0 { y = y + x; x = x - 1;}") = [("x", Value.NumV 0); ("y", Value.NumV 6)]
let%test _ = Hw13.interp_prog (ParserMain.parse "x = 3; y = 10;") = [("y", Value.NumV 10); ("x", Value.NumV 3)]
let%test _ = Hw13.interp_prog (ParserMain.parse "x = 3; y = 7; z = x < y; if z { z = y - x;}") = [("z", Value.NumV 4); ("y", Value.NumV 7); ("x", Value.NumV 3)]

(* additional test *)
let%test _ = Hw13.interp_prog (ParserMain.parse "x = 3; y = 7; z = x < y; if z { z = y - x;}") = [("z", Value.NumV 4); ("y", Value.NumV 7); ("x", Value.NumV 3)]

let%test _ = try Hw12.interp (ParserMain.parse "if x < 1 then 2 else 3") Store.empty = Store.NumV 0 with Failure msg -> msg = "[Error] Free identifier: x"
let%test _ = try Hw12.interp (ParserMain.parse "let foo = (fun x -> x + 1) in if foo < 3 then 20 else 200") Store.empty = Store.NumV 0 with Failure msg -> msg = "[Error] Not a number: foo < 3"
let%test _ = try Hw12.interp (ParserMain.parse "let rec foo = 1 in foo") Store.empty = Store.NumV 0 with Failure msg -> msg = "[Error] Not a function: 1"
