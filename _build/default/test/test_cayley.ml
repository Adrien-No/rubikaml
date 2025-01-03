open Lib

let is_test = ref false
let _ =
  Printf.printf "%s\n%!" (Sys.getcwd());
  let cmll =
    (try Grapher.read_algs "datas/cmll.txt"
     with Sys_error _ -> is_test := true; Grapher.read_algs "../../../datas/cmll-extended.txt")
    (* |> (fun l -> List.iter (fun (name, alg) -> Printf.printf "(%s, %s)\n" name alg) l; l) *)
    |> List.map snd |> List.map (fun algo -> String.split_on_char ' ' algo) in

  let g = Cayley.graph_of_algs cmll in
  let eq c c' =
    let open Simulator in
    if is_cmll c && is_cmll c' then
      c = c' || c = u c' || c = (c' |> u |> u) || c = u' c'
    else c === c'
  in
  let g = Lib.Grapher.merge_vertex_equality eq g in
  let file_out = open_out ((if !is_test then "../../../" else "") ^"graph.dot") in
  Lib.Grapher.dot_of_graph file_out g
