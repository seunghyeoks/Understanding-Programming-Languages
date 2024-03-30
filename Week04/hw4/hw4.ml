type token = TI of int | TO of char
type state = Q0 | Q1 | Q2

(* String에서 공백을 제외한 글자를 하나씩 뽑아 char list로 반환하는 함수 *)
let toCharList (s : string) : char list = 
  let s = String.to_seq s in 
  let s = List.of_seq s in 
  let s = List.filter (fun x -> (x != ' ')) s in
  s

(* 숫자 char만 들어있는 char list를 TI(int)로 변환하는 함수 *)
let listToToken (lst : char list) : token =
  match lst with
  | [] -> failwith "Failed in Lexing"
  | a :: _ -> 
    match a with
    | '+' | '-' | '*' | '/' -> TO a
    | _ ->  let lst = List.to_seq lst in
            let lst = String.of_seq lst in
            let lst = int_of_string lst in
            TI lst



(* 재귀로 구현 *)
let lex (s : string) : token list =
  let rec lex' lst q b1 b2 = 
    match q with 
    | Q0 -> 
      (match lst with
      | [] -> failwith "Failed in Lexing"
      | a :: t -> 
        (match a with
        | '0' .. '9' -> lex' t Q1 (b1 @ [a]) b2  
        | _ -> failwith "Failed in Lexing"))
    | Q1 -> 
      (match lst with
      | [] -> b2 @ [listToToken b1]
      | a :: t -> 
        (match a with
        | '0' .. '9' -> lex' t Q1 (b1 @ [a]) b2  
        | '+' | '-' | '*' | '/' -> lex' t Q2 [a] (b2 @ [listToToken b1])
        | _ -> failwith "Failed in Lexing"))
    | Q2 -> 
      (match lst with
      | [] -> b2 @ [listToToken b1]
      | a :: t -> 
        (match a with
        | '0' .. '9' -> lex' t Q1 [a] (b2 @ [listToToken b1])
        | _ -> failwith "Failed in Lexing"))
  in lex' (toCharList s) Q0 [] []