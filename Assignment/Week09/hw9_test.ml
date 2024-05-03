let%test _ = Hw9.interp_prog (ParserMain.parse "def f1 x y = x + y endef def f2 x y = x - y endef f1(3, 4) + f2(4, 7)") = Store.NumV 4
let%test _ = Hw9.interp_prog (ParserMain.parse "def add_one x = x + 1 endef def sub x y = x - y endef sub(add_one(5), 3)") = Store.NumV 3
let%test _ = Hw9.interp_prog (ParserMain.parse "def add x y = x + y endef def three z = z + z + z endef three(add(-1, -1))") = Store.NumV (-6)

(* additonal test 
let%test _ = Hw9.interp_prog (ParserMain.parse "3 + 1") = Store.NumV 4
let%test _ = Hw9.interp_prog (ParserMain.parse "3 - 5") = Store.NumV (-2)
let%test _ = Hw9.interp_prog (ParserMain.parse "let x = 1 in x") = Store.NumV 1
let%test _ = Hw9.interp_prog (ParserMain.parse "def f1 x y = x + y endef f1(3, 4)") = Store.NumV 7
let%test _ = Hw9.interp_prog (ParserMain.parse "def all x y z = x + y endef all(1, 2, 3)") = Store.NumV 3
let%test _ = Hw9.interp_prog (ParserMain.parse "def add_one x = x + 1 endef def add_two x = x + 2 endef def add_three x = x + 3 endef add_three(add_two(add_one(0)))") = Store.NumV 6

let%test _ = try Hw9.interp_prog (ParserMain.parse "def f1 x y = z + y endef f1(3, 4)") = Store.NumV 7 with Failure msg -> msg = "[Error] Free identifier: z"
let%test _ = try Hw9.interp_prog (ParserMain.parse "def f1 x y = x + y endef f(3, 4)") = Store.NumV 7 with Failure msg -> msg = "[Error] Unbound function: f"
let%test _ = try Hw9.interp_prog (ParserMain.parse "def f1 x y = x + y endef f1(3, 4, 7)") = Store.NumV 7 with Failure msg -> msg = "[Error] invalid argument number"
*)