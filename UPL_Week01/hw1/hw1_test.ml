(* success case *)
let%test _ = Hw1.calc '+' 1 2 = 3
let%test _ = Hw1.calc '-' 1 3 = -2
let%test _ = Hw1.calc '*' 1 4 = 4
let%test _ = Hw1.calc '/' 1 5 = 0
let%test _ = try Hw1.calc '%' 1 2 = 0 with Failure msg -> msg = "Unsupported operation" (* try catch style *)


(* failed case *)
let%test _ = Hw1.calc '+' 1 2 = 4
let%test _ = Hw1.calc '-' 1 3 = -5
let%test _ = Hw1.calc '*' 1 4 = 8
let%test _ = Hw1.calc '/' 1 5 = 12
let%test _ = try Hw1.calc '%' 1 2 = 0 with Failure msg -> msg = "Unsupported"