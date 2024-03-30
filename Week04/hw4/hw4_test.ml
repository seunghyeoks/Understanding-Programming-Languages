(* lex *)
let%test _ = Hw4.lex "1 + 2" = [Hw4.TI 1; Hw4.TO '+'; Hw4.TI 2]