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
  TI lst


let lex (s : string) : token list = 
  let charlst : char list = toCharList s in
  let buffer1 : char list = [] in
  let buffer2 : token list = [] in 
  let _ = List.iter (fun x -> 
            match x with
            | ('0' .. '9') -> buffer1 @ [x]
            | '+' -> begin buffer2 = buffer2 @ [toIntToken buffer1] ; buffer1 = [] ; buffer2 = buffer2 @ [x] end
            | '-' -> begin buffer2 = buffer2 @ [toIntToken buffer1] ; buffer1 = [] ; buffer2 = buffer2 @ [x] end
            | '*' -> begin buffer2 = buffer2 @ [toIntToken buffer1] ; buffer1 = [] ; buffer2 = buffer2 @ [x] end
            | '/' -> begin buffer2 = buffer2 @ [toIntToken buffer1] ; buffer1 = [] ; buffer2 = buffer2 @ [x] end
            | _   -> failwith "Failed in Lexing"
          ) charlst in 
  let _ = List.iter (Format.printf "%a ") buffer2