(* FVAE를 해석하는 interpreter *)

let rec interp (e : Ast.expr) (t : Store.t) : Store.value = 
  match e with
  | Ast.Num (n) -> Store.NumV n
  | Ast.Id (x) -> Store.find x t
  | Ast.Add (e1, e2) -> 
    let n1 = interp e1 t in
    let n2 = interp e2 t in
    begin 
      match n1, n2 with
      | NumV n1, NumV n2 -> NumV (n1 + n2)
      | _ -> failwith (Format.asprintf "[Error] Not a number: %a + %a" Ast.pp e1 Ast.pp e2)
    end
  | Ast.Sub (e1, e2) -> 
    let n1 = interp e1 t in
    let n2 = interp e2 t in
    begin 
      match n1, n2 with
      | NumV n1, NumV n2 -> NumV (n1 - n2)
      | _ -> failwith (Format.asprintf "[Error] Not a number: %a - %a" Ast.pp e1 Ast.pp e2)
    end
  | Ast.LetIn (x, e1, e2) -> 
    let v1 = interp e1 t in
    let v2 = interp e2 (Store.add x v1 t) in
    v2
  | Ast.Lambda (x, e) -> ClosureV (x, e, t)
  | Ast.App (e1, e2) ->
    let f1 = interp e1 t in
    let v1 = interp e2 t in
    begin 
      match f1 with 
      | NumV (_) -> failwith (Format.asprintf "[Error] Not a function: %a" Ast.pp e1)
      | ClosureV (x, e3, t1) -> let v2 = interp e3 (Store.add x v1 t1) in v2
    end