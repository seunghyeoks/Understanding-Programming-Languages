let%test _ = Hw11.interp (ParserMain.parse "if 0 < 1 then 2 else 3") Store.empty = Store.NumV 2
let%test _ = Hw11.interp (ParserMain.parse "let x = 1 in if x < 3 then 10 else 100") Store.empty = Store.NumV 10
let%test _ = Hw11.interp (ParserMain.parse "let foo = (fun x -> x + 1) in if (foo 1) < 3 then 20 else 200") Store.empty = Store.NumV 20

(* additional test
let%test _ = Hw11.interp (ParserMain.parse "let y = 2 in let f = (fun x -> x + y) in let y = 3 in if y < 3 then (f (y+1)) else (f y)") Store.empty = Store.NumV 5
let%test _ = Hw11.interp (ParserMain.parse "let y = 2 in let x = 4 in if x < y then y else x") Store.empty = Store.NumV 4
let%test _ = Hw11.interp (ParserMain.parse "if 1 < 2 then let x = 3 in x else let y = 8 in y") Store.empty = Store.NumV 3
let%test _ = Hw11.interp (ParserMain.parse "if 3 < 2 then let x = 3 in x else let y = 8 in y") Store.empty = Store.NumV 8

let%test _ = try Hw11.interp (ParserMain.parse "if x < 1 then 2 else 3") Store.empty = Store.NumV 0 with Failure msg -> msg = "[Error] Free identifier: x"
let%test _ = try Hw11.interp (ParserMain.parse "let foo = (fun x -> x + 1) in if foo < 3 then 20 else 200") Store.empty = Store.NumV 0 with Failure msg -> msg = "[Error] Not a number: foo < 3"
*)