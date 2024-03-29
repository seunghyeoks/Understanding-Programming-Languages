(* factorial *)
let%test _ = Hw2.factorial 5 = 120
let%test _ = Hw2.factorial 0 = 1
let%test _ = Hw2.factorial (-10) = (-1)

(* fibonacci number *)
let%test _ = Hw2.fib 5 = 8
let%test _ = Hw2.fib 0 = 1
let%test _ = Hw2.fib (-10) = (-1)

(* accumulate *)
let%test _ = Hw2.acc 4 = 10
let%test _ = Hw2.acc 0 = 0
let%test _ = Hw2.acc (-5) = (-15)

(* power *)
let%test _ = Hw2.pow 2 5 = 32
let%test _ = Hw2.pow 0 0 = 1
let%test _ = Hw2.pow (-1) 1 = 0