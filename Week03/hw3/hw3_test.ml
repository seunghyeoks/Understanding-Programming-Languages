(* map *)
let%test _ = Hw3.map (fun x -> x + 1) [1;2;3] = [2;3;4]
let%test _ = Hw3.map (fun x -> (string_of_int x) ^ "_string") [1;2;3] = ["1_string";"2_string";"3_string"]
let%test _ = Hw3.map (fun x -> x * x) [5;8;10;12] = [25;64;100;144]

(* fold_left *)
let%test _ = Hw3.fold_left (fun i x -> x + i) 0 [1;2;3] = 6
let%test _ = Hw3.fold_left (fun i x -> x * i) 3 [2;3;4] = 72
let%test _ = Hw3.fold_left (fun i x -> i ^ x) "concatenate_" ["A";"B";"C"] = "concatenate_ABC"