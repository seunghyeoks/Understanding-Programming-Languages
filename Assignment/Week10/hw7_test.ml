let%test _ = Hw7.interp (ParserMain.parse "x + y") [("x", NumV 1); ("y", NumV 2)] = Store.NumV 3
let%test _ = Hw7.interp (ParserMain.parse "let x = 10 in let y = 3 in let z = 20 in x + y - z") Store.empty = Store.NumV (-7)
let%test _ = try Hw7.interp (ParserMain.parse "let x = 3 in z + 1") Store.empty = Store.NumV 4 with Failure msg -> msg = "[Error] Free identifier: z"

(* additional test 
let%test _ = ParserMain.parse "let x = 3 in x + 1" = Ast.LetIn ("x", Ast.Num 3, Ast.Add (Ast.Id "x", Ast.Num 1))

let%test _ = Store.find "x" [("a", NumV 1); ("x", NumV 5); ("y", NumV (-10))] = NumV 5
let%test _ = try Store.find "x" [] = NumV 0 with Failure msg -> msg = "[Error] Free identifier: x"
let%test _ = try Store.find "y" Store.empty = NumV 0 with Failure msg -> msg = "[Error] Free identifier: y"

let%test _ = Store.add "x" (NumV 5) Store.empty = [("x", NumV 5)]
let%test _ = Store.add "x" (NumV 5) [("v2", NumV 10); ("yer", NumV (-20))] = [("x", NumV 5); ("v2", NumV 10); ("yer", NumV (-20))]
let%test _ = Store.add "x" (NumV 5) [("v2", NumV 10); ("yer", NumV (-20)); ("x", NumV 12)] = [("x", NumV 5); ("v2", NumV 10); ("yer", NumV (-20))]

let%test _ = Hw7.interp (ParserMain.parse "x") [("x", NumV 1)] = Store.NumV 1
let%test _ = Hw7.interp (ParserMain.parse "x + y") [("x", NumV 1); ("y", NumV 2)] = Store.NumV 3
let%test _ = Hw7.interp (ParserMain.parse "let x = 10 in let y = 3 in x + y") Store.empty = Store.NumV 13
let%test _ = Hw7.interp (ParserMain.parse "let x = 3 in x + y") [("x", NumV 1); ("y", NumV 2)] = Store.NumV 5
*)