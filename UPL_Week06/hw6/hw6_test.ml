let%test _ = Hw6.interp (ParserMain.parse "1 + 2 - 3") = Value.NumV 0
let%test _ = Hw6.interp (ParserMain.parse "12 - 34") = Value.NumV (-22)
let%test _ = Hw6.interp (ParserMain.parse "1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10") = Value.NumV 55

(* additional test *)