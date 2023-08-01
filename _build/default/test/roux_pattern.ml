Printexc.record_backtrace true

open Local_deps
open Types

let _ =
  let pattern = Simulator.fresh_pattern() in
(* first block *)
  pattern.corners_opt.(3) <- Some (Green,Yellow,Orange);
  pattern.corners_opt.(6) <- Some (Blue,Yellow,Orange);
  pattern.edges_opt.(3) <- Some (Green,Orange);
  pattern.edges_opt.(11) <- Some (Yellow,Orange);
  pattern.edges_opt.(5) <- Some (Blue,Orange);
  pattern.centers_opt.(4) <- Some (Orange);
(*  *)
  let starting_cube = Simulator.solved () in
  Scramble.scramble starting_cube ~print:true;
  Simulator.print_faces starting_cube;
  let cube',moves = Solver.solve starting_cube pattern in
  Simulator.print_sequence moves;
  Simulator.print_faces cube'

(* Second block *)
  (* pattern.corners_opt.(2) <- Some (Green,Yellow,Red); *)
  (* pattern.corners_opt.(7) <- Some (Blue,Yellow,Red); *)
  (* pattern.edges_opt.(8) <- Some (Green,Red); *)
  (* pattern.edges_opt.(10) <- Some(Yellow,Red); *)
  (* pattern.edges_opt.(7) <- Some (Blue,Red); *)
  (* pattern.centers_opt.(5) <- Some (Red); *)

  (* let cube'',moves = Solver.solve cube' pattern in *)
  (* Simulator.print_sequence moves; *)
  (* Simulator.print_faces cube'' *)
