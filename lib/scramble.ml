open Types

let _ = Random.self_init()
let scramble_length = 5
let moves = [|F;F';B;B';U;U';D;D';L;L';R;R'|]

let get_scramble_sequence () : sequence =
  List.init scramble_length (fun _ -> moves.(Random.int (Array.length moves)))
