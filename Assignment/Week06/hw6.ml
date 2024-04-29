(* AST를 해석하여 결과값을 계산하는 interpreter, 7주차에 많이 다듬었으니 7주차로 *)

let addV (v1:Value.t) (v2:Value.t) : Value.t = 
  match v1, v2 with
  | NumV v1, NumV v2 -> NumV (v1 + v2)

let subV (v1:Value.t) (v2:Value.t) : Value.t = 
  match v1, v2 with
  | NumV v1, NumV v2 -> NumV (v1 - v2)
  

let rec interp (e : Ast.expr) : Value.t = 
  match e with
  | Ast.Num (n) -> Value.NumV n
  | Ast.Add (e1, e2) -> 
    (match e1, e2 with 
    | Ast.Num e1, Ast.Num e2 -> addV (Value.NumV e1) (Value.NumV e2)
    | _         , Ast.Num e2 -> addV (interp e1) (Value.NumV e2)
    | Ast.Num e1, _          -> addV (Value.NumV e1) (interp e2)
    | _                      -> addV (interp e1) (interp e2)
    )
  | Ast.Sub (e1, e2) ->
    (match e1, e2 with 
    | Ast.Num e1, Ast.Num e2 -> subV (Value.NumV e1) (Value.NumV e2)
    | _         , Ast.Num e2 -> subV (interp e1) (Value.NumV e2)
    | Ast.Num e1, _          -> subV (Value.NumV e1) (interp e2)
    | _                      -> subV (interp e1) (interp e2)
    )