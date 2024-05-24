let%test _ = Hw12.interp (ParserMain.parse "let rec sum = (fun x -> if 0 < x then x + (sum (x - 1)) else 0) in sum 10") Store.empty = Store.NumV 55
let%test _ = Hw12.interp (ParserMain.parse "let rec sum = (fun x -> if x < 0 then x + (sum (x + 1)) else 0) in sum (-10)") Store.empty = Store.NumV (-55)
let%test _ = Hw12.interp (ParserMain.parse "let rec battle = (fun x y -> if x < y then x + (battle (x + 1) y) else 0) in battle 5 10") Store.empty = Store.NumV 35

(* additional test *)
let%test _ = Hw12.interp (ParserMain.parse "let y = 2 in let f = (fun x -> x + y) in let y = 3 in if y < 3 then (f (y+1)) else (f y)") Store.empty = Store.NumV 5
let%test _ = Hw12.interp (ParserMain.parse "let y = 2 in let x = 4 in if x < y then y else x") Store.empty = Store.NumV 4
let%test _ = Hw12.interp (ParserMain.parse "if 1 < 2 then let x = 3 in x else let y = 8 in y") Store.empty = Store.NumV 3
let%test _ = Hw12.interp (ParserMain.parse "if 3 < 2 then let x = 3 in x else let y = 8 in y") Store.empty = Store.NumV 8

let%test _ = try Hw12.interp (ParserMain.parse "if x < 1 then 2 else 3") Store.empty = Store.NumV 0 with Failure msg -> msg = "[Error] Free identifier: x"
let%test _ = try Hw12.interp (ParserMain.parse "let foo = (fun x -> x + 1) in if foo < 3 then 20 else 200") Store.empty = Store.NumV 0 with Failure msg -> msg = "[Error] Not a number: foo < 3"
let%test _ = try Hw12.interp (ParserMain.parse "let rec foo = 1 in foo") Store.empty = Store.NumV 0 with Failure msg -> msg = "[Error] Not a function: 1"
