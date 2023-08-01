open Array

(* Contains some useful function to write properly. *)

let currying2 (f:'a*'b->'c) : 'a->'b->'c = fun x -> fun y -> f (x,y)

let uncurrying2 (f:'a->'b->'c) : ('a*'b)->'c = fun (x,y) -> f x y

let currying3 f = fun x -> fun y -> fun z -> f (x,y,z)

let uncurrying3 f = fun (x,y,z) -> f x y z

let for_all3 p l1 l2 l3 =
  let n1 = length l1
  and n2 = length l2
  and n3 = length l3 in
  if n1 <> n2 || n2 <> n3 || n1 <> n3 then (Printf.printf "%i %i %i" n1 n2 n3; invalid_arg "Array.for_all3")
  else let rec loop i =
    if i = n1 then true
    else if p (get l1 i) (get l2 i) (get l3 i) then loop (succ i)
    else false in
  loop 0
