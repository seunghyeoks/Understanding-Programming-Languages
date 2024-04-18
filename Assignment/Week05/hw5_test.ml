(* parse *)
let%test _ = Hw5.parse (Hw5.lex "1+2-3") = E (SUB, (E (ADD, Num 1, Num 2)), Num 3)
let%test _ = Hw5.parse (Hw5.lex "1+2*3") = E (ADD, Num 1, (E (MUL, Num 2, Num 3)))
let%test _ = Hw5.parse (Hw5.lex "1 * 361 - 89 + 2 / 56") = E (ADD, (E (SUB, (E (MUL, Num 1, Num 361)), Num 89)), (E (DIV, Num 2, Num 56)))
let%test _ = Hw5.parse (Hw5.lex "1 * 2 / 3 + 4 - 5") = E (SUB, (E (ADD, (E (DIV, (E (MUL, Num 1, Num 2)), Num 3)), Num 4)), Num 5)
let%test _ = try Hw5.parse (Hw5.lex "1+2*") = (Num 1) with Failure msg -> msg = "Failed in Parsing"


(* additional parse test
let%test _ = try Hw5.parse (Hw5.lex "1++") = (Num 1) with Failure msg -> msg = "Failed in Lexing"
let%test _ = try Hw5.parse (Hw5.lex "-1*2") = (Num 1) with Failure msg -> msg = "Failed in Lexing"
let%test _ = try Hw5.parse (Hw5.lex "1+") = (Num 1) with Failure msg -> msg = "Failed in Parsing"
let%test _ = Hw5.parse (Hw5.lex "1 * 2 / 3") = E (DIV, (E (MUL, Num 1, Num 2)), Num 3)
let%test _ = Hw5.parse (Hw5.lex "1 * 2 + 3 / 4") = E (ADD, (E (MUL, Num 1, Num 2)), (E (DIV, Num 3, Num 4)))
*)