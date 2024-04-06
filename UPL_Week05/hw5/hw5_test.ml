(* lex *)
let%test _ = Hw5.lex "1 * 361 - 89 + 2 / 56" = [Hw5.TI 1; Hw5.TO '*'; Hw5.TI 361; Hw5.TO '-'; Hw5.TI 89; Hw5.TO '+'; Hw5.TI 2; Hw5.TO '/'; Hw5.TI 56]
let%test _ = Hw5.lex "0 * 1   2 - 0   23 /" = [Hw5.TI 0; Hw5.TO '*'; Hw5.TI 12; Hw5.TO '-'; Hw5.TI 23; Hw5.TO '/']
let%test _ = try Hw5.lex "3 3 2 * * 2" = [] with Failure msg -> msg = "Failed in Lexing"

(* additional test *)
let%test _ = Hw5.lex "1 +  " = [Hw5.TI 1; Hw5.TO '+']
let%test _ = Hw5.lex "1   2" = [Hw5.TI 12]

let%test _ = Hw5.lex "1 * 361 - 89 + 2 /" = [Hw5.TI 1; Hw5.TO '*'; Hw5.TI 361; Hw5.TO '-'; Hw5.TI 89; Hw5.TO '+'; Hw5.TI 2; Hw5.TO '/']

let%test _ = try Hw5.lex "1 ++ 2" = [] with Failure msg -> msg = "Failed in Lexing"
let%test _ = try Hw5.lex "3 3 2 * / 2" = [] with Failure msg -> msg = "Failed in Lexing"
let%test _ = try Hw5.lex "+23 - 1" = [] with Failure msg -> msg = "Failed in Lexing"
let%test _ = try Hw5.lex "1 + 2.0" = [] with Failure msg -> msg = "Failed in Lexing"
let%test _ = try Hw5.lex "1 % 2" = [] with Failure msg -> msg = "Failed in Lexing"

(* parse *)
let%test _ = Hw5.parse (Hw5.lex "1") = Num 1
let%test _ = Hw5.parse (Hw5.lex "1+2") = E (ADD, Num 1, Num 2)
let%test _ = Hw5.parse (Hw5.lex "1+2-3") = E (SUB, (E (ADD, Num 1, Num 2)), Num 3)
let%test _ = Hw5.parse (Hw5.lex "1+2*3") = E (ADD, Num 1, (E (MUL, Num 2, Num 3)))