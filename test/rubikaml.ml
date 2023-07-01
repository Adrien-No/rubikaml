open Local_deps
open Local_deps.Rudimentary_parser
let _ =
  while true do
    let cube = Simulator.solved() in
    ask_action cube
  done
