(* MiniC를 해석하는 interpreter *)

let rec interp_expr (e : Ast.expr) (em : Env.t * Memory.t) : Value.t = 
  match em, e with
  | (_, _), Ast.Num (n) -> Value.NumV n

  | (_, _), Ast.Bool (b) -> Value.BoolV b

  | (s, _), Ast.Ref (x) -> Value.AddrV (Env.find x s)

  | (s, m), Ast.Id (x) -> Memory.find (Env.find x s) m

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
      | AddrV v1, AddrV v2 -> if v1 = v2 then (Value.BoolV true) else (Value.BoolV false)
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



let rec interp_stmt (t : Fstore.t) (stmt : Ast.stmt) (em : Env.t * Memory.t) : Env.t * Memory.t = 
  match em, stmt with
  | (s, m), Ast.DefStmt (x) -> 
    let a = AddrManager.new_addr () in
    let new_s = Env.add x a s in 
    (new_s, m)

  | (s, m), Ast.LoadStmt (x, e) ->
    let sx = Env.find x s in
    let a = interp_expr e (s, m) in
    begin 
      match a with
      | AddrV a -> 
        let ma = Memory.find a m in
        let new_m = Memory.add sx ma m in
        (s, new_m)
      | _ -> failwith (Format.asprintf "[Error] Not an Address: %a" Ast.pp_expr e)
    end

  | (s, m), Ast.StoreStmt (e1, e2) ->
    let a = interp_expr e1 (s, m) in 
    let v = interp_expr e2 (s, m) in
    begin
      match a with
      | AddrV a -> let new_m = Memory.add a v m in (s, new_m)
      | _ -> failwith (Format.asprintf "[Error] Not an Address: %a" Ast.pp_expr e1)
    end

  | (s, m), Ast.IfStmt (e, sl1, sl2) ->
    let b = interp_expr e (s, m) in
    begin
      match b with
      | BoolV b -> 
        if b then 
          let (_, m1) = interp_stmtlist t sl1 (s, m) in (s, m1) 
        else 
          let (_, m1) = interp_stmtlist t sl2 (s, m) in (s, m1) 
      | _ -> failwith (Format.asprintf "[Error] Not a Bool: %a" Ast.pp_expr e)
    end

  | (s, m), Ast.LoopStmt (e, sl) -> 
    let eb = interp_expr e (s, m) in
    begin 
      match eb with
      | BoolV b -> 
        if not b then (s, m) else 
          let (_, m1) = interp_stmtlist t sl (s, m) in 
          let (_, m2) = interp_stmt t (Ast.LoopStmt (e, sl)) (s, m1) in 
          (s, m2)
      | _ -> failwith (Format.asprintf "[Error] Not a Bool: %a" Ast.pp_expr e)
    end

  | (s, m), Ast.ReturnStmt (e) ->
    let v = interp_expr e (s, m) in
    let new_m = Memory.add AddrManager.ret_addr v m in
    (s, new_m)

  | (s, m), Ast.CallStmt (x, f, el) ->
    let (xl, sl) = Fstore.find f t in
    let rec interp_emlist (em : Env.t * Memory.t) (el : Ast.expr list) (xl : string list) (acc : Env.t * Memory.t) : Env.t * Memory.t =
      begin
        match em, el, xl, acc with
          | (s,m), e1 :: e_remain, x1 :: x_remain, (acc_s, acc_m) -> 
            let a1 = AddrManager.new_addr () in
            let v1 = interp_expr e1 (s,m) in
            let acc_s = Env.add x1 a1 acc_s in
            let acc_m = Memory.add a1 v1 acc_m in
            interp_emlist (s,m) e_remain x_remain (acc_s, acc_m)
          | _, [], [], (acc_s, acc_m) -> (acc_s, acc_m)
          | _, _, _, _ -> failwith (Format.asprintf "[Error] arguments mismatch") 
      end in
    let (s_to_n, m_to_n) = interp_emlist (s, m) el xl (s, m) in
    let (_, mp) = interp_stmtlist t sl (s_to_n, m_to_n) in
    let sx = Env.find x s in
    let mpret = Memory.find AddrManager.ret_addr mp in
    let new_mp = Memory.add sx mpret mp in
    (s, new_mp)
      
(* let, rec 다 빼야 됨 *)
and interp_stmtlist (t : Fstore.t) (sl : Ast.stmt list) (em : Env.t * Memory.t) : Env.t * Memory.t = 
  match sl with
  | fst :: remain -> let new_em = interp_stmt t fst em in interp_stmtlist t remain new_em
  | [] -> em



let interp_def (d : Ast.def) (t : Fstore.t) : Fstore.t = 
  match d with
  | Ast.FunDef (f, xl, stl) -> Fstore.add f xl stl t



let interp_prog (p : Ast.prog) : Env.t * Memory.t = 
  let _ = AddrManager.init () in
  match p with
  | Ast.Program (defl, stmtl) -> 
    let lamda = List.fold_right interp_def defl Fstore.empty in
    let (s, m) = interp_stmtlist lamda stmtl (Env.empty, Memory.empty) in
    (s, m)
