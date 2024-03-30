(* lex *)
let%test _ = Hw4.lex "1 * 361 - 89 + 2 / 56" = [Hw4.TI 1; Hw4.TO '*'; Hw4.TI 361; Hw4.TO '-'; Hw4.TI 89; Hw4.TO '+'; Hw4.TI 2; Hw4.TO '/'; Hw4.TI 56]
let%test _ = Hw4.lex "0 * 1   2 - 0   23 /" = [Hw4.TI 0; Hw4.TO '*'; Hw4.TI 12; Hw4.TO '-'; Hw4.TI 23; Hw4.TO '/']
let%test _ = try Hw4.lex "3 3 2 * * 2" = [] with Failure msg -> msg = "Failed in Lexing"

(* additional test *)
let%test _ = Hw4.lex "1 +  " = [Hw4.TI 1; Hw4.TO '+']
let%test _ = Hw4.lex "1   2" = [Hw4.TI 12]

let%test _ = Hw4.lex "1 * 361 - 89 + 2 /" = [Hw4.TI 1; Hw4.TO '*'; Hw4.TI 361; Hw4.TO '-'; Hw4.TI 89; Hw4.TO '+'; Hw4.TI 2; Hw4.TO '/']

let%test _ = try Hw4.lex "1 ++ 2" = [] with Failure msg -> msg = "Failed in Lexing"
let%test _ = try Hw4.lex "3 3 2 * / 2" = [] with Failure msg -> msg = "Failed in Lexing"
let%test _ = try Hw4.lex "+23 - 1" = [] with Failure msg -> msg = "Failed in Lexing"
let%test _ = try Hw4.lex "1 + 2.0" = [] with Failure msg -> msg = "Failed in Lexing"
let%test _ = try Hw4.lex "1 % 2" = [] with Failure msg -> msg = "Failed in Lexing"