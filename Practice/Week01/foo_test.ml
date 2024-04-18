(* Test Code*)

(* Success *)
let%test _ = Foo.foo 2 = 2

(* failed *)
let%test _ = Foo.foo 2 = 3