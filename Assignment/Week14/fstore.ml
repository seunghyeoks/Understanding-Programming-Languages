type t = (string * string list * Ast.stmt list) list

let empty : t = [] 

let add (s : string) (arg : string list) (body : Ast.stmt list) (t : t) : t = 
  let t = List.filter (fun x -> 
    match x with
    | (var, _, _) -> 
      if var = s then false
      else true
    ) t 
  in (s, arg, body) :: t

let rec find (s : string) (t : t) : (string list * Ast.stmt list) = 
  match t with
  | [] -> failwith ("[Error] Unbound function: " ^ s)
  | fst :: remain -> 
    (match fst with
    | (var, arg, body) -> 
      if var = s then (arg, body)
      else find s remain
    )