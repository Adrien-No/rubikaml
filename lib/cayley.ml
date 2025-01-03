(* We print cayley-graph for some subset of rubik's cube *)

(* given a set of algs supposed to conduct to a same goal, we build a graph that contain configurations as vertex and moves as edges *)
let graph_of_algs algs =
  let open Grapher in
  List.fold_left (fun g l ->
      List.fold_left (fun (g, c) move ->
          let ancestor = c |> Solver.move_of_string (Solver.inv move) in
          let g = G.add_vertex g ancestor in
          let g = G.add_edge_e g (G.E.create ancestor move c) in
          (g, ancestor)
        ) (g, Simulator.create()) (List.rev l) |> fst
    ) G.empty algs
