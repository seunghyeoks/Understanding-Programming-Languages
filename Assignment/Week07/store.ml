type value = NumV of int
type t = (string * value) list

let empty : t = [] 

let rec find (s : string) (t : t) : value = 
  match t with
  | [] -> failwith ("[Error] Free identifier: " ^ s)
  | fst :: remain -> 
    (match fst with
    | (var, num) -> 
      if var = s then num
      else find s remain
    )

let add (s : string) (v : value) (t : t) : t = 
  let t = List.filter (fun x -> 
    match x with
    | (var, _) -> 
      if var = s then false
      else true
    ) t 
  in (s, v) :: t