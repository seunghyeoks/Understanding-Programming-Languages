let%test _ = Hw10.interp (ParserMain.parse "(fun x y -> x + y) 1 3") Store.empty = Store.NumV 4
let%test _ = Hw10.interp (ParserMain.parse "let f = (fun x y -> x + y) in f 1 3") Store.empty = Store.NumV 4
let%test _ = Hw10.interp (ParserMain.parse "let foo = (fun x -> (fun y -> y + x)) in foo 2 3") Store.empty = Store.NumV 5

(* additional test 
let%test _ = Hw10.interp (ParserMain.parse "let foo = (fun x -> x + 1) in let bar = (fun x -> x 1) in (bar foo)") Store.empty = Store.NumV 2
let%test _ = Hw10.interp (ParserMain.parse "let foo = (fun x -> x + 1) in let bar = (fun x -> foo x) in (foo (bar 3))") Store.empty = Store.NumV 5
let%test _ = Hw10.interp (ParserMain.parse "let x = 0 in let f = (fun y -> y + x) in let x = 99 in f 1") Store.empty = Store.NumV 1
let%test _ = Hw10.interp (ParserMain.parse "let y = 2 in let f = (fun x -> x + y) in let y = 3 in (f y)") Store.empty = Store.NumV 5

let%test _ = try Hw10.interp (ParserMain.parse "let f = (fun x -> x + 1) in f + 1") Store.empty = Store.NumV 0 with Failure msg -> msg = "[Error] Not a number: f + 1"
let%test _ = try Hw10.interp (ParserMain.parse "(fun x -> x + 1) - 1") Store.empty = Store.NumV 0 with Failure msg -> msg = "[Error] Not a number: (fun x -> x + 1) - 1"
let%test _ = try Hw10.interp (ParserMain.parse "1 1") Store.empty = Store.NumV 0 with Failure msg -> msg = "[Error] Not a function: 1"
let%test _ = try Hw10.interp (ParserMain.parse "v 1") Store.empty = Store.NumV 0 with Failure msg -> msg = "[Error] Free identifier: v"
let%test _ = try Hw10.interp (ParserMain.parse "let v = 1 in v 1") Store.empty = Store.NumV 0 with Failure msg -> msg = "[Error] Not a function: let v = 1 in v"
let%test _ = try Hw10.interp (ParserMain.parse "let v = 1 in (v 1)") Store.empty = Store.NumV 0 with Failure msg -> msg = "[Error] Not a function: v"
*)