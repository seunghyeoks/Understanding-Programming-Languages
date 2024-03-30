type token = TI of int | TO of char

(* String에서 공백을 제외한 글자를 하나씩 뽑아 char list로 반환하는 함수 *)
let toCharList (s : string) : char list = 
  let s = String.to_seq s in 
  let s = List.of_seq s in 
  let s = List.filter (fun x -> (x != ' ')) s in
  s

(* 숫자 char만 들어있는 char list를 TI(int)로 변환하는 함수 *)
let listToTI (lst : char list) : token =
  let lst = List.to_seq lst in
  let lst = String.of_seq lst in
  let lst = int_of_string lst in
  TI lst

(* 재귀로 구현 *)
let lex (s : string) : token list =  
  let rec lex' lst b1 b2 = 
    match lst with
    | [] -> 
            if b1 = [] then b2  
            else b2 @ [listToTI b1] 
    | a :: t -> 
            match a with
            | '0' .. '9' -> lex' t (b1 @ [a]) b2  
            | '+' | '-' | '*' | '/' ->  let _ = b2 @ [listToTI b1] in 
                                        let b1 = [] in                 
                                        lex' t b1 (b2 @ [TO a])       
            | _ -> failwith "Failed with Lexing"
  in lex' (toCharList s) [] []