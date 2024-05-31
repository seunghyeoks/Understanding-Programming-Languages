(* MiniC를 해석하는 interpreter *)

let rec interp_expr (e : Ast.expr) (t : Store.t) : Value.t = 
  match e with
  | Ast.Num (n) -> Value.NumV n

  | Ast.Bool (b) -> Value.BoolV b

  | Ast.Name (x) -> Store.find x t

  | Ast.Add (e1, e2) -> 
    let n1 = interp_expr e1 t in
    let n2 = interp_expr e2 t in
    begin 
      match n1, n2 with
      | NumV n1, NumV n2 -> NumV (n1 + n2)
      | _ -> failwith (Format.asprintf "[Error] Not a Number: %a + %a" Ast.pp_expr e1 Ast.pp_expr e2)
    end
    
  | Ast.Sub (e1, e2) -> 
    let n1 = interp_expr e1 t in
    let n2 = interp_expr e2 t in
    begin 
      match n1, n2 with
      | NumV n1, NumV n2 -> NumV (n1 - n2)
      | _ -> failwith (Format.asprintf "[Error] Not a Number: %a - %a" Ast.pp_expr e1 Ast.pp_expr e2)
    end

  | Ast.Lt (e1, e2) ->
    let n1 = interp_expr e1 t in
    let n2 = interp_expr e2 t in
    begin 
      match n1, n2 with
      | NumV n1, NumV n2 -> if n1 < n2 then (Value.BoolV true) else (Value.BoolV false)
      | _ -> failwith (Format.asprintf "[Error] Not a Number: %a < %a" Ast.pp_expr e1 Ast.pp_expr e2)
    end

  | Ast.Gt (e1, e2) ->
    let n1 = interp_expr e1 t in
    let n2 = interp_expr e2 t in
    begin 
      match n1, n2 with
      | NumV n1, NumV n2 -> if n1 > n2 then (Value.BoolV true) else (Value.BoolV false)
      | _ -> failwith (Format.asprintf "[Error] Not a Number: %a > %a" Ast.pp_expr e1 Ast.pp_expr e2)
    end

  | Ast.Eq (e1, e2) ->
    let v1 = interp_expr e1 t in
    let v2 = interp_expr e2 t in
    begin 
      match v1, v2 with
      | NumV v1, NumV v2 -> if v1 = v2 then (Value.BoolV true) else (Value.BoolV false)
      | BoolV v1, BoolV v2 -> if v1 = v2 then (Value.BoolV true) else (Value.BoolV false)
      | _ -> (Value.BoolV false)
    end

  | Ast.And (e1, e2) ->
    let b1 = interp_expr e1 t in
    let b2 = interp_expr e2 t in
    begin 
      match b1, b2 with
      | BoolV v1, BoolV v2 -> BoolV (v1 && v2)
      | _ -> failwith (Format.asprintf "[Error] Not a Bool: %a && %a" Ast.pp_expr e1 Ast.pp_expr e2)
    end

  | Ast.Or (e1, e2) ->
    let b1 = interp_expr e1 t in
    let b2 = interp_expr e2 t in
    begin 
      match b1, b2 with
      | BoolV v1, BoolV v2 -> BoolV (v1 || v2)
      | _ -> failwith (Format.asprintf "[Error] Not a Bool: %a || %a" Ast.pp_expr e1 Ast.pp_expr e2)
    end



let rec interp_stmt (s : Ast.stmt) (t : Store.t) : Store.t = 
  match s with
  | Ast.AssignStmt (x, e) ->
    let v = interp_expr e t in Store.add x v t

  | Ast.IfStmt (e, sl1, sl2) ->
    let eb = interp_expr e t in
    begin
      match eb with
      | BoolV b -> if b then interp_stmtlist t sl1 else interp_stmtlist t sl2
      | _ -> failwith (Format.asprintf "[Error] Not a Bool: %a" Ast.pp_expr e)
    end

  | Ast.LoopStmt (e, sl) -> 
    let eb = interp_expr e t in
    begin 
      match eb with
      | BoolV b -> if not b then t else let t1 = interp_stmtlist t sl in let t2 = interp_stmt (Ast.LoopStmt (e, sl)) t1 in t2
      | _ -> failwith (Format.asprintf "[Error] Not a Bool: %a" Ast.pp_expr e)
    end

(* let, rec 다 빼야 됨 *)
and interp_stmtlist (t: Store.t) (sl : Ast.stmt list) : Store.t = 
  match sl with
  | fst :: remain -> let new_t = interp_stmt fst t in interp_stmtlist new_t remain
  | [] -> t


let interp_prog (p : Ast.prog) : Store.t = 
  match p with
  | Ast.Program (sl) -> interp_stmtlist Store.empty sl

