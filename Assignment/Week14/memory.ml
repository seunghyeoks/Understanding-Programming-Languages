type t = (Env.addr * Value.t) list

let empty : t = []

let add (a : Env.addr) (v : Value.t) (t : t) : t =



let rec find (a : Env.addr) (t : t) : Value.t =