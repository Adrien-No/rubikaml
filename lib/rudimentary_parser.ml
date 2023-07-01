open Types

let char_to_move : char -> move = function
    'F' -> F
  | 'B' -> B
  | 'U' -> U
  | 'D' -> D
  | 'L' -> L
  | 'R' -> R
  | _ -> failwith "unknown mouvement"

let char_to_move' : char -> move = function
    'F' -> F'
  | 'B' -> B'
  | 'U' -> U'
  | 'D' -> D'
  | 'L' -> L'
  | 'R' -> R'
  | _ -> failwith "unknown mouvement"

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

let apply_scramble cube scramble =
  Simulator.exec_sequence cube scramble ;
  Printf.printf "We apply theses movements to cube : ";
  Simulator.print_sequence scramble;
  Simulator.print_faces cube

let ask_action cube =
  Printf.printf "What should I do ?\n";
  Printf.printf "1 - random scramble\n";
  Printf.printf "2 - given scramble\n";
  let rec understand = function
      "1" -> apply_scramble cube (Scramble.get_scramble_sequence ())
    | "2" -> apply_scramble cube (read_seq_entry())
    | _ -> Printf.printf "%s" "I don't understand\n"; understand (read_line())
  in
  understand (read_line())
