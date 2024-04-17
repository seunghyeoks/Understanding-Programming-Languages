let _ =
  let arr = [1;2;3;4;5;6] in
    let res = List.filter 
      (fun x -> (x mod 2) = 0) arr 
    in
    List.iter 
      (fun x -> Format.printf "%d " x) res