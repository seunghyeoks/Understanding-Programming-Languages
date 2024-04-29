(* finite automata 구현 *)

type op = ADD | SUB | MUL | DIV
type value = Int of int | Float of float | Err
type expr = E of op * expr * expr | V of value



(* float operation function*)
let foperate op l r =
  match op with
  | ADD -> Float (l +. r)
  | SUB -> Float (l -. r)
  | MUL -> Float (l *. r)
  | DIV -> if r = 0.0 then Err else Float (l /. r)



(* expr -> value, 일종의 interpreter 같기도 하고 *)
let rec eval expr =
  match expr with
  | V v -> v
  | E (oper, left, right) ->
    let left_val = eval left in
    let right_val = eval right in
    match (left_val, right_val) with
    | (Err, _) | (_, Err) -> Err
    | (Int l, Int r) ->
      (match oper with
       | ADD -> Int (l + r)
       | SUB -> Int (l - r)
       | MUL -> Int (l * r)
       | DIV -> if r = 0 then Err else Int (l / r))
    | (Int l, Float r) ->
        let l = float_of_int l in
        foperate oper l r
    | (Float l, Int r) ->
        let r = float_of_int r in
        foperate oper l r
    | (Float l, Float r) ->
        foperate oper l r