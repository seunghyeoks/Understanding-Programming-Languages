type t = (Env.addr * Value.t) list

let empty : t = []

let add (a : Env.addr) (v : Value.t) (t : t) : t =
  let t = List.filter (fun x -> 
    match x with
    | (ref, _) -> 
      if ref = a then false
      else true
    ) t 
  in (a, v) :: t


let rec find (a : Env.addr) (t : t) : Value.t =
  match t with
  | [] -> failwith ("[Error] Free address: " ^ (string_of_int a))
  | fst :: remain -> 
    (match fst with
    | (ref, v) -> 
      if ref = a then v
      else find a remain
    )