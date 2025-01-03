open Lib

let _ =
  let open Simulator in
  let c = create() in
  let c' = create() |> u |> f |> f |> b |> d |> d in
  Solver.pure_path (=) c c' |> Solver.path_to_string |> print_string
