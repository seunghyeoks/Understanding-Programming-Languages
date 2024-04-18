let map f lst = 
  let rec map' f lst rt = 
    match lst with
    | [] -> rt
    | a :: t -> map' f t (rt @ [f a])
  in map' f lst []

let rec fold_left f init lst = 
  match lst with 
    | [] -> init
    | a :: t -> fold_left f (f init a) t