open Lib

let is_test = ref true
let _ =
  Printf.printf "%s\n%!" (Sys.getcwd());
  (* let cmll = *)
  (*   (try Lib.Grapher.read_algs "datas/cmll.txt" *)
  (*    with Sys_error _ -> is_test := true; Lib.Grapher.read_algs "../../../datas/cmll.txt") *)
  (*   (\* |> (fun l -> List.iter (fun (name, alg) -> Printf.printf "(%s, %s)\n" name alg) l; l) *\) *)
  (*   |> List.map snd |> List.map (fun algo -> String.split_on_char ' ' algo) in *)

  (* let g = Lib.Cayley.graph_of_algs [["L"]] in *)
  let g = Grapher.G.empty in
  let g = Grapher.G.add_vertex g (Simulator.create()) in
  let file_out = open_out ((if !is_test then "../../../" else "") ^"graph.dot") in
  Lib.Grapher.dot_of_graph file_out g
