type t = (string * string list * Ast.stmt list) list

let empty : t = [] 

let add (s : string) (sl : string list) (stl : Ast.stmt list) (t : t) : t = 
  let t = List.filter (fun x -> 
    match x with
    | (var, _) -> 
      if var = s then false
      else true
    ) t 
  in (s, v) :: t

let rec find (s : string) (t : t) : (string list * Ast.stmt list) = 
  match t with
  | [] -> failwith ("[Error] Free identifier: " ^ s)
  | fst :: remain -> 
    (match fst with
    | (var, num) -> 
      if var = s then num
      else find s remain
    )