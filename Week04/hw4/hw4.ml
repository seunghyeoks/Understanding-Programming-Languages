type token = TI of int | TO of char


let toCharList (s : string) : char list = 
  let s = String.to_seq s in 
  let s = List.of_seq s in 
  let s = List.filter (fun x -> (x != ' ')) s in
  s

let toIntToken (lst : char list) : token =
  let lst = List.to_seq lst in
  let lst = String.of_seq lst in
  let lst = int_of_string lst in
  TO lst


let lex (s : string) : token list = 
  let charlst : char list = toCharList s  in
  let buffer1 : char list = [] in
  let buffer2 : token list = [] in 
    let _ = List.iter (fun x : token list-> 
              match x with
              | ('0' .. '9') -> buffer1 @ [x]
              | '+' -> buffer2 @ [buffer1]
              | '-' -> buffer2 @ [TO x]
              | '*' -> buffer2 @ [TO x]
              | '/' -> buffer2 @ [TO x]
              | _   -> failwith "Failed in Lexing"
            ) charlst in !buffer2
