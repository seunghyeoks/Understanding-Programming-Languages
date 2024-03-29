let calc (w : char) (x : int) (y : int) : int = 
  if w = '+' then x + y else 
  if w = '-' then x - y else
  if w = '*' then x * y else
  if w = '/' then x / y else
  failwith "Unsupported operation"