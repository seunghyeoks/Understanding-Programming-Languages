(* 추상메모리(Λ)에 대한 구현 *)

type t = (string * (string list * Ast.expr)) list

let empty : t = [] 

(* 추상메모리 t에서 변수 s에 맞는 함수찾아, 인자리스트와 몸체를 반환*)
let rec find (s : string) (t : t) : string list * Ast.expr = 
  match t with
  | [] -> failwith ("[Error] Unbound function: " ^ s)
  | fst :: remain -> 
    (match fst with
    | (name, (argu, body)) -> 
      if name = s then (argu, body)
      else find s remain
    )

(* 추상메모리 t에서 변수 s에 맞는 함수를 업데이트한 새로운 메모리를 반환 *)
(* 기존 t에 이미 s가 있을 경우, 그것을 제거하고 새로운 함수를 추가 *)
let add (s : string) (v : string list) (body : Ast.expr) (t : t) : t = 
  let t = List.filter (fun x -> 
    match x with
    | (var, _) -> 
      if var = s then false
      else true
    ) t 
  in (s, (v, body)) :: t