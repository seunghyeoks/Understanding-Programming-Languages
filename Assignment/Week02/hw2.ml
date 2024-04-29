(* if then else, 패턴매칭, 재귀를 이용한 4가지 함수
   -> tail call optimization이 안되어 있음 *)
   
let rec factorial i = 
  if i < 0 then -1 else
  match i with
  | 0 -> 1
  | n -> n * factorial (n - 1)

let rec fib i = 
  if i < 0 then -1 else
  match i with
  | 0 -> 1
  | 1 -> 1
  | n -> fib (n - 2) + fib (n - 1)

let rec acc i = 
  if i < 0 
  then match i with 
  | 0 -> 0 
  | n -> n + acc (n + 1)
  else match i with
  | 0 -> 0
  | n -> n + acc (n - 1)

let rec pow i p = 
  if i < 0 || p < 0 then 0 else
  if p = 0 then 1 
  else i * pow i (p - 1)