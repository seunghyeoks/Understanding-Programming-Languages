(* 추상메모리에 대한 구현 *)

type value = NumV of int
type t = (string * value) list

let empty : t = [] 

(* 추상메모리 t에서 변수 s에 맞는 값을 반환*)
let rec find (s : string) (t : t) : value = 
  match t with
  | [] -> failwith ("[Error] Free identifier: " ^ s)
  | fst :: remain -> 
    (match fst with
    | (var, num) -> 
      if var = s then num
      else find s remain
    )

(* 추상메모리 t에서 변수 s에 값을 v로 업데이트한 새로운 메모리를 반환 *)
(* 기존 t에 이미 s가 있을 경우, 그것을 제거하고 새로운 (s, v)를 추가 *)
let add (s : string) (v : value) (t : t) : t = 
  let t = List.filter (fun x -> 
    match x with
    | (var, _) -> 
      if var = s then false
      else true
    ) t 
  in (s, v) :: t