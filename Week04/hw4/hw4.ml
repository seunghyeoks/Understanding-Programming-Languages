type token = TI of int | TO of char

let toCharList (s : string) : char list = 
  let s = String.to_seq s in 
  let s = List.of_seq s in 
  let s = List.filter (fun x -> (x != ' ')) s in
  s



let lex (s : string) : token list = 
  let buffer1 : char list = toCharList s in
  let buffer2 : token list = [] in 
  let _ = List.iter (fun x -> buffer2 @ [x]) buffer1 in 
  let _ = List.iter (Format.printf "%c ") buffer2