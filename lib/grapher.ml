let read_algs file_name =
  let file = Scanf.Scanning.open_in file_name in
  let algs = ref [] in
  try
    while true do
      let alg_name, alg =
        Scanf.bscanf file "%s@ : %[^@\n]\n" (fun name alg -> name, alg)
        (* Scanf.bscanf file "%s : %s" (fun name alg -> name, alg) in *)
      in
      algs := (alg_name, alg) :: !algs
    done;
  with End_of_file ->
    Scanf.Scanning.close_in file;
    !algs


module Node : (Graph.Sig.COMPARABLE with type t = Simulator.t) = struct
   type t = Simulator.t
   let compare = compare
   let hash = Hashtbl.hash
   let equal = (=)
end

(* representation of an edge -- must be comparable *)
module Edge = struct
   type t = string
   let compare = compare
   let equal = (=)
   let default = ""
end

(* a functional/persistent graph *)
module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Node)(Edge)

(* module for creating dot-files *)
module Dot = Graph.Graphviz.Neato (struct

   include G (* use the graph module from above *)
   let edge_attributes (_a, label, _b) =
     let int_of_move = function
       | "F" | "F'" -> 255
       | "B" | "B'" -> 65280
       | "U" | "U'" | "U2" -> 16776960
       | "D" | "D'" -> 13882323
       | "L" | "L'" -> 16753920
       | "R" | "R'" -> 16711680
       | _ -> 0
    in
    [`Label label; `Color (int_of_move label)]

   let default_edge_attributes _ = []
   let get_subgraph _ = None

   let vertex_attributes c =
     if Simulator.is_cmll c then (* TODO mettre dans un foncteur et donner l'egalite en arg -> en fait pas besoin car is_cmll donne le mm res que wide_cmll *)
         [`Label (Simulator.to_cmll c); `Shape `Circle]
       else
         [`Height 0.; `Label ""; `Width 0.]

   let vertex_name c = Simulator.to_int c |> string_of_int

  let default_vertex_attributes _ = []
  let graph_attributes _ = [`Spline true]
end)

let dot_of_graph file g =
  Dot.output_graph file g

module Merger = Graph.Merge.P(G)
(* merge vertices of a graph g depending of an equality gave in argument *)
let merge_vertex_equality eq (g : G.t) =
  let rec build_classes (cls : (Node.t * Node.t list) list)  = function
    | [] -> cls
    | vtx::q -> begin
      match List.find_opt (fun (rpz, _) -> eq rpz vtx) cls with
      | None -> build_classes ((vtx, [vtx])::cls) q
      | Some (rpz, cl) ->
        let cls = List.map (function (rpz', cl) when rpz' = rpz -> (rpz, vtx::cl) | x -> x) cls in
        build_classes cls q
      end
  in
  let vertices_to_merge = build_classes [] (G.fold_vertex List.cons g []) |> List.map snd in
  List.fold_left (fun g cl -> Merger.merge_vertex g cl) g vertices_to_merge

(* let g = Merger.merge_vertex *)
