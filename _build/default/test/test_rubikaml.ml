open Lib

let rec exec_seq show c = function
  | [] -> ()
  | move::queue -> let c = move c in
    if show then Printf.printf "%s\n\n" (Simulator.to_string c);
    exec_seq show c queue

let _ =
  let open Simulator in
  let c = create() in exec_seq true c []
  (* exec_seq true c [d'; f; l; r'; f; r; u; l'; u; b; b; r'; d; l; f; u'; r; b'; d; l'; d] *)
    (* test : d'; f; l; r'; f; r; u; l'; u; b; b; r'; d; l; f; u'; r; b'; d; l'; d *)

  (* Printf.printf "%s\n\n" (S.to_string c); *)
  (* let c = S.f c in *)
  (* Printf.printf "%s\n\n" (S.to_string c); *)
  (* let c = S.f c in *)
  (* Printf.printf "%s\n\n" (S.to_string c); *)
  (* let c = S.f c in *)
  (* Printf.printf "%s\n\n" (S.to_string c) *)
