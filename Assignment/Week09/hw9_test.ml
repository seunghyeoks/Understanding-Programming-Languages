let%test _ = Hw9.interp_prog (ParserMain.parse "def f1 x y = x + y endef def f2 x y = x - y endef f1(3, 4) + f2(4, 7)") = Store.NumV 4
let%test _ = Hw9.interp_prog (ParserMain.parse "def add_one x = x + 1 endef def sub x y = x - y endef sub(add_one(5), 3)") = Store.NumV 3
let%test _ = Hw9.interp_prog (ParserMain.parse "def add x y = x + y endef def three x = x + x + x endef three(add(-1, -1))") = Store.NumV (-6)


