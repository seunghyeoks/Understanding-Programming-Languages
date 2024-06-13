(* F1VAE를 해석하는 interpreter 만들기 *)
(* First-order fucntion *)

(* 기존 interp에서 call을 확장 *)
let rec interp_expr (e : Ast.expr) (f : Fstore.t) (s: Store.t) : Store.value = 
  match e with
  | Ast.Num (n) -> Store.NumV n
  | Ast.Id (i) -> Store.find i s
  | Ast.Add (e1, e2) -> 
    let NumV n1 = interp_expr e1 f s in
    let NumV n2 = interp_expr e2 f s in
    NumV (n1 + n2)
  | Ast.Sub (e1, e2) -> 
    let NumV n1 = interp_expr e1 f s in
    let NumV n2 = interp_expr e2 f s in
    NumV (n1 - n2)
  | Ast.LetIn (x, e1, e2) -> 
    let n1 = interp_expr e1 f s in
    let NumV n2 = interp_expr e2 f (Store.add x n1 s) in 
    NumV n2
  | Ast.Call (x, e_list) -> 
    let n_list = List.map (fun ek -> interp_expr ek f s) e_list in
    let (x_list, e2) = Fstore.find x f in 
    if List.length n_list <> List. length x_list then failwith "[Error] invalid argument number"
    else let temp_s = List.fold_left2 (fun s x n -> Store.add x n s) [] x_list n_list in
    let NumV n2 = interp_expr e2 f temp_s in 
    NumV n2


(* def에 대한 해석 추가 *)
let interp_def (d : Ast.fundef) (f : Fstore.t) : Fstore.t = 
  match d with
  | Ast.FunDef (name, para, expr) -> Fstore.add name para expr f


(* 앞서 작성한 두 함수를 사용하는 대표 함수 *)
let interp_prog (p : Ast.prog) : Store.value = 
  match p with
  | Ast.Prog (d_list, e) -> 
    let temp_f = List.fold_left (fun f d -> interp_def d f) [] d_list in
    let NumV n = interp_expr e temp_f [] in 
    NumV n