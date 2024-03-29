let len lst = 
  let rec len' lst acc = 
    match lst with
    | [] -> acc
    | _ :: t -> len' t (acc+1)
  in len' lst 0

let rev (lst : 'a list) : 'a list =
  let rec rev' lst acc = 
    match lst with
    | [] -> acc
    | a :: t -> rev' t (a :: acc)
  in rev' lst []
