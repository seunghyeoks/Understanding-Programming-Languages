(* MiniC를 해석하는 interpreter *)

let rec interp_expr (e : Ast.expr) (em : Env.t * Memory.t) : Value.t = 
  match em, e with
  | (_, _), Ast.Num (n) -> Value.NumV n

  | (_, _), Ast.Bool (b) -> Value.BoolV b

  | (s, _), Ast.Ref (r) -> s.find x s

  | (s, m), Ast.Id (x) -> m.find (s.find x s) m

  | (s, m), Ast.Add (e1, e2) -> 
    let n1 = interp_expr e1 (s, m) in
    let n2 = interp_expr e2 (s, m) in
    begin 
      match n1, n2 with
      | NumV n1, NumV n2 -> NumV (n1 + n2)
      | _ -> failwith (Format.asprintf "[Error] Not a Number: %a + %a" Ast.pp_expr e1 Ast.pp_expr e2)
    end
    
  | (s, m), Ast.Sub (e1, e2) -> 
    let n1 = interp_expr e1 (s, m) in
    let n2 = interp_expr e2 (s, m) in
    begin 
      match n1, n2 with
      | NumV n1, NumV n2 -> NumV (n1 - n2)
      | _ -> failwith (Format.asprintf "[Error] Not a Number: %a - %a" Ast.pp_expr e1 Ast.pp_expr e2)
    end

  | (s, m), Ast.Lt (e1, e2) ->
    let n1 = interp_expr e1 (s, m) in
    let n2 = interp_expr e2 (s, m) in
    begin 
      match n1, n2 with
      | NumV n1, NumV n2 -> if n1 < n2 then (Value.BoolV true) else (Value.BoolV false)
      | _ -> failwith (Format.asprintf "[Error] Not a Number: %a < %a" Ast.pp_expr e1 Ast.pp_expr e2)
    end

  | (s, m), Ast.Gt (e1, e2) ->
    let n1 = interp_expr e1 (s, m) in
    let n2 = interp_expr e2 (s, m) in
    begin 
      match n1, n2 with
      | NumV n1, NumV n2 -> if n1 > n2 then (Value.BoolV true) else (Value.BoolV false)
      | _ -> failwith (Format.asprintf "[Error] Not a Number: %a > %a" Ast.pp_expr e1 Ast.pp_expr e2)
    end

  | (s, m), Ast.Eq (e1, e2) ->
    let v1 = interp_expr e1 (s, m) in
    let v2 = interp_expr e2 (s, m) in
    begin 
      match v1, v2 with
      | NumV v1, NumV v2 ->   if v1 = v2 then (Value.BoolV true) else (Value.BoolV false)
      | BoolV v1, BoolV v2 -> if v1 = v2 then (Value.BoolV true) else (Value.BoolV false)
      | Addr v1, Addr v2 ->   if v1 = v2 then (Value.BoolV true) else (Value.BoolV false)
      | _ -> (Value.BoolV false)
    end

  | (s, m), Ast.And (e1, e2) ->
    let b1 = interp_expr e1 (s, m) in
    let b2 = interp_expr e2 (s, m) in
    begin 
      match b1, b2 with
      | BoolV b1, BoolV b2 -> BoolV (b1 && b2)
      | _ -> failwith (Format.asprintf "[Error] Not a Bool: %a && %a" Ast.pp_expr e1 Ast.pp_expr e2)
    end

  | (s, m), Ast.Or (e1, e2) ->
    let b1 = interp_expr e1 (s, m) in
    let b2 = interp_expr e2 (s, m) in
    begin 
      match b1, b2 with
      | BoolV b1, BoolV b2 -> BoolV (b1 || b2)
      | _ -> failwith (Format.asprintf "[Error] Not a Bool: %a || %a" Ast.pp_expr e1 Ast.pp_expr e2)
    end




let rec interp_emlist (em : Env.t * Memory.t) (el : Ast.expr list) (xl : string list) : Env.t * Memory.t =
  match em , el, xl with
  | (s, m), e :: e_remain, x :: x_remain -> 
    let v = interp_expr e (s, m) in
    let a = AddrManager.new_addr in
    let new_s = Env.add x1 a s in
    let new_m = Memory.add a v m in
    interp_emlist (new_s, new_m) e_remain x_remain
  | (s, m), [], [] -> (s, m)
  | _, _, _ -> failwith (Format.asprintf "[Error]")


let rec interp_stmt (t : Fstore.t) (stmt : Ast.stmt) (em : Env.t * Meomry.t) : Env.t * Memory.t = 
  match em, stmt with
  | (s, m), Ast.DefStmt (x) -> 
    let a = AddrManager.new_addr in
    let s1 = s.add x a s in 
    (s1, m)

  | (s, m), Ast.StoreStmt (e1, e2) ->
    let a = interp_expr e1 (s, m) in
    let v = interp_expr e2 (s, m) in
    let m1 = m.add a v m1 in
    (s, m1)


  | (s, m), Ast.LoadStmt (x, e) ->
    let sx = s.find x s in
    let a = interp_expr e (s, m) in
    let ma = m.find a m in
    let m2 = m.add sx ma m in
    (s, m2)

  | (s, m), Ast.IfStmt (e, sl1, sl2) ->
    let eb = interp_expr e (s, m) in
    begin
      match eb with
      | BoolV b -> 
        if b then 
          let (s1, m1) = interp_stmtlist (s, m) sl1 else 
          let (s1, m1) = interp_stmtlist (s, m) sl2 in
          (s, m1)
      | _ -> failwith (Format.asprintf "[Error] Not a Bool: %a" Ast.pp_expr e)
    end

  | (s, m), Ast.LoopStmt (e, sl) -> 
    let eb = interp_expr e (s, m) in
    begin 
      match eb with
      | BoolV b -> 
        if not b then (s, m) else 
          let (s1, m1) = interp_stmtlist (s, m) sl in 
          let (s2, m2) = interp_stmt t (Ast.LoopStmt (e, sl)) (s, m1) in 
          (s, m2)
      | _ -> failwith (Format.asprintf "[Error] Not a Bool: %a" Ast.pp_expr e)
    end

  | (s, m), Ast.ReturnStmt (e) ->
    let v = interp_expr e (s, m) in
    let new_m = Memory.add AddrManager.ret_addr v m in
    (s, new_m)

  | (s, m), Ast.CallStmt (str1, str2, el) ->
    let (xl, sl) = Fstore.find str2 t in    
    let (sp, mp) = interp_emlist (s, m) el xl in
    let sx = Env.find x s in
    let mpret = Memory.find AddrManager.ret_addr mp in
    let new_mp = Memory.add sx mpret mp in
    (s, new_mp)

(* let, rec 다 빼야 됨 *)
and interp_stmtlist (em : Env.t * Memory.t) (sl : Ast.stmt list) : Env.t * Memory.t = 
  match em, sl with
  | (s, m), fst :: remain -> let new_em = interp_stmt (s, m) t in interp_stmtlist new_em remain
  | (_, _), [] -> (s, m)


let interp_def (d : Ast.def) (t : Fstore.t) : Fstore.t = 
  match d with
  | (f, xl, stl) -> Fstore.add f xl stl t

let interp_prog (p : Ast.prog) : Env.t * Memory.t = 
  match p with
  | Ast.Program (dl, stl) -> 
    let s = interp_def 

  (* fold left??? *)

