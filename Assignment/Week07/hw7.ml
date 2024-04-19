let rec interp (e : Ast.expr) (t : Store.t) : Store.value = 
  match e with
  | Ast.Num (n) -> Store.NumV n
  | Ast.Id (i) -> Store.find i t
  | Ast.Add (e1, e2) -> 
    let NumV n1 = interp e1 t in
    let NumV n2 = interp e2 t in
    NumV (n1 + n2)
  | Ast.Sub (e1, e2) -> 
    let NumV n1 = interp e1 t in
    let NumV n2 = interp e2 t in
    NumV (n1 - n2)
  | Ast.LetIn (s, e1, e2) -> 
    let t = Store.add s (interp e1 t) t in 
    (interp e2 t)