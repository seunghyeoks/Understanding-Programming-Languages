let _ = 
  let lst = [1;2;3;4;5] in
  let lst' = List.map (fun x -> x + 1) lst in
  let lst'' = List.fold_left (fun i x -> i + x) 0 lst in
  let _ = List.iter (fun x -> Format.printf "%d " x) lst in
  let _ = List.iter (fun x -> Format.printf "%d " x) lst' 
in Format.printf "%d" lst'' 