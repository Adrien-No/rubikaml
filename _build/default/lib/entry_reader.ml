open Types

let char_to_move : char -> move = function
    'F' -> F
  | 'B' -> B
  | 'U' -> U
  | 'D' -> D
  | 'L' -> L
  | 'R' -> R
  | _ -> failwith "mouvement inconnu"

let char_to_move' : char -> move = function
    'F' -> F'
  | 'B' -> B'
  | 'U' -> U'
  | 'D' -> D'
  | 'L' -> L'
  | 'R' -> R'
  | _ -> failwith "mouvement inconnu"

let read_seq_entry () : sequence =
  let s = read_line() in
  let l = List.init (String.length s) (String.get s) in

  let rec parser (l:char list) : sequence =
    match l with
      [] -> []
    | t1::t2::q -> if t2 = '\'' then char_to_move' t1 :: parser q
                   else char_to_move t1 :: parser (t2::q)
    | t::_ -> [char_to_move t]
  in
  parser l
