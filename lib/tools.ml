(** Contains some useful function to write properly. *)

let currying (f:'a*'b->'c) : 'a->'b->'c = fun x -> fun y -> f (x,y)

let uncurrying (f:'a->'b->'c) : ('a*'b)->'c = fun (x,y) -> f x y
