(* if-then-else 문을 사용한 int용 사칙 연산기 구현 *)

let calc (w : char) (x : int) (y : int) : int = 
  if w = '+' then x + y else 
  if w = '-' then x - y else
  if w = '*' then x * y else
  if w = '/' then x / y else
  failwith "Unsupported operation"