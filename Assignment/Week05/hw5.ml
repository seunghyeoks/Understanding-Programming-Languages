(* lexer를 통해 토큰화된 프로그램을 AST로 변환하는 parser 만들기 *)

type token = TI of int | TO of char
type state = Q0 | Q1 | Q2
type op = ADD | SUB | MUL | DIV
type expr = E of op * expr * expr | Num of int
type stack_elem = Token of token | Expr of expr | Op of op



(* String에서 공백을 제외한 글자를 하나씩 뽑아 char list로 반환하는 함수 *)
let toCharList (s : string) : char list = 
  let s = String.to_seq s in 
  let s = List.of_seq s in 
  let s = List.filter (fun x -> (x <> ' ')) s in
  s
  

(* char list의 맨 앞 요소를 확인하여 알맞은 토큰으로 변환하는 함수 *)
let listToToken (b1 : char list) : token =
  match b1 with
  | [] -> failwith "Failed in Lexing"
  | a :: _ -> 
    match a with
    | '+' | '-' | '*' | '/' -> TO a         (* TO 토큰의 경우 *)
    | _ ->  let b1 = List.to_seq b1 in      (* TI 토큰의 경우 *)
            let b1 = String.of_seq b1 in
            let b1 = int_of_string b1 in
            TI b1



(* lexer: 재귀와 삼중 타입 매칭으로 구현 *)
let lex (s : string) : token list =
  let rec lex' lst q b1 b2 = 
    match q with 
    | Q0 -> 
      (match lst with
      | [] -> failwith "Failed in Lexing"         (*  fail at q0  *)
      | a :: t -> 
        (match a with
        | '0' .. '9' -> lex' t Q1 (b1 @ [a]) b2   (*  (q0,[0-9]) -> q1   *)
        | _ -> failwith "Failed in Lexing"))      (*  fail at q0  *)

    | Q1 -> 
      (match lst with
      | [] -> b2 @ [listToToken b1]               (*  final at q1 *)
      | a :: t -> 
        (match a with
        | '0' .. '9' -> lex' t Q1 (b1 @ [a]) b2   (*  (q1,[0-9]) -> q1  *)
        | '+' | '-' | '*' | '/' -> lex' t Q2 [a] (b2 @ [listToToken b1])  (*  (q1,[+|-|*|/]) -> q2  *)
        | _ -> failwith "Failed in Lexing"))      (*  fail at q1  *)

    | Q2 -> 
      (match lst with
      | [] -> b2 @ [listToToken b1]               (*  final at q2  *)
      | a :: t -> 
        (match a with
        | '0' .. '9' -> lex' t Q1 [a] (b2 @ [listToToken b1]) (*  (q2,[0-9]) -> q1  *)
        | _ -> failwith "Failed in Lexing"))      (*  fail at q2  *)

  in lex' (toCharList s) Q0 [] []



(* parser: 스택에 대한 pattern matching으로 구현 *)
let parse (tl : token list) : expr = 
  let rec parse' (tl : token list) (stck : stack_elem list) : expr = 
    match stck with
    | Token (TI n) :: stck_remain ->  (* reduce *)
      parse' tl (Expr (Num n) :: stck_remain)

    | Token (TO c) :: stck_remain ->  (* reduce *)
      (match c with
      | '+' -> parse' tl (Op ADD :: stck_remain)
      | '-' -> parse' tl (Op SUB :: stck_remain)
      | '*' -> parse' tl (Op MUL :: stck_remain)
      | '/' -> parse' tl (Op DIV :: stck_remain) 
      | _ -> parse' tl (Op DIV :: stck_remain) )

    | Expr e1 :: Op op :: Expr e2 :: stck_remain -> 
      (match op with
      | ADD | SUB ->
        (match tl with
        | t :: tl_remain ->     (* lookahead *)
          (match t with 
          | TO '*' | TO '/' -> parse' tl_remain (Token t :: stck)   (* shift *)
          | _ -> parse' tl (Expr (E (op, e2, e1)) :: stck_remain))  (* reduce *)
        | _ -> parse' tl (Expr (E (op, e2, e1)) :: stck_remain))  (* reduce *)
      | _ -> parse' tl (Expr (E (op, e2, e1)) :: stck_remain))  (* reduce *)

    | _ ->  
      (match tl with
      | t :: tl_remain -> parse' tl_remain (Token t :: stck)   (* shift *) 
      | [] ->         (* final *)
        (match stck with
        | Expr e :: [] -> e  (* SUCCESS *)
        | _ -> failwith "Failed in Parsing"))  (* FAILED *)

  in parse' tl []