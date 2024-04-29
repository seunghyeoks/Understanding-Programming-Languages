(* 재귀를 통해 List 라이브러리의 Map과 Fold_left를 구현해보기 *)

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