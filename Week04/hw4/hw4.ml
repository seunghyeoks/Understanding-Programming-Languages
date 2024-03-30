type token = TI of int | TO of char

let toCharList (s : string) : char list = 
  let s = String.to_seq s in 
  let s = List.of_seq s in 
  let s = List.filter (fun x -> (x != ' ')) s in
  s

let listToTI (lst : char list) : token =
  let lst = List.to_seq lst in
  let lst = String.of_seq lst in
  let lst = int_of_string lst in
  TI lst


let lex (s : string) : token list = 
  let charlst : char list = toCharList s in
  let buffer1 : char list = [] in
  let buffer2 : token list = [] in 
  let rec lex' lst b1 b2 = 
    match lst with
    | [] -> b2
    | a :: t -> 
      match a with
      | '0' .. '9' -> lex' t (b1 @ [a]) b2
      | '+' | '-' | '*' | '/' ->
        let b2_ = b2 @ [listToTI b1] in
        let b1_ = [] in
        lex' t b1_ (b2_ @ [TO a])
      | _ -> failwith "Failed with Lexing"
  in lex' charlst buffer1 buffer2
