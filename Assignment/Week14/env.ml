type addr = int
type t = (string * addr) list

let empty : t = []

let add (s : string) (a : addr) (t : t) : t = 
  let t = List.filter (fun x -> 
    match x with
    | (var, _) -> 
      if var = s then false
      else true
    ) t 
  in (s, a) :: t

let rec find (s : string) (t : t) : addr = 
  match t with
  | [] -> failwith ("[Error] Free identifier: " ^ s)
  | fst :: remain -> 
    (match fst with
    | (var, ref) -> 
      if var = s then ref
      else find s remain
    )