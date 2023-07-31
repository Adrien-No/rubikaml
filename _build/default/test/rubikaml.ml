open Local_deps
open Local_deps.Rudimentary_parser
let _ =
  (* Activate the "interactiv mode" *)
  flush stdout;
  let cube = Simulator.solved() in
  ask_action cube
