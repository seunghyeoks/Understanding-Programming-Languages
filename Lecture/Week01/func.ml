let x = (fun x -> x + 1) 3 in
Format.printf "%d\n" x;

let f = (fun x y -> x + y) in
Format.printf "%d\n" (f 3 4);

(* syntactic sugar *)
let sum x y = x + y in
Format.printf "%d\n" (sum 3 7)

(* 시퀀싱 때문에 세미콜론 추가 *)
(* 중간에 다른 코드 넣거나 반환값을 unit이 아니게 하면 
   시퀀싱이 깨지므로 세미콜론 없어도 됨 *)