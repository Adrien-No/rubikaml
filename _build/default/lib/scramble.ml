open Types
open Simulator

(* let _ = Random.self_init() *)
let scramble_length = 15
let moves = [|F;F';B;B';U;U';D;D';L;L';R;R'|]

let get_scramble_sequence () : sequence =
  List.init scramble_length (fun _ -> moves.(Random.int (Array.length moves)))

(** [scramble c] apply a scramble on [c]. *)
let scramble ?(print=false) (c:cube) : unit =
  let seq = get_scramble_sequence() in
  if print then print_sequence seq;
  exec_sequence c seq
