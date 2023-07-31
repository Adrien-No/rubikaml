open Types
open Simulator

let k = 10000

let cube_matching (pattern:matching_cube) (cube:cube) : bool =
  (** Compare a cube with a cube-pattern ie the configuration that we wants.*)
  let array_test = function
    None -> fun _ -> true
  | Some x -> (=)x
  in
  Array.for_all2 array_test pattern.centers_opt cube.centers
  && Array.for_all2 array_test pattern.edges_opt cube.edges
  && Array.for_all2 array_test pattern.corners_opt cube.corners

let cube_matching_range (pattern:matching_cube) (cube:cube) : int =
  (** Pretty similar as cube_matching but returns the number of missplaced cubies. *)
  (** Slower at returning 0 than cube_matching at returning false. *)
  let array_test = function
    None -> fun _ -> 0
  | Some x -> fun y -> if x = y then 0 else 1
  in
  Array.fold_left (fun init (x,y) -> init + array_test x y) 0 (Array.combine pattern.centers_opt cube.centers)
  + Array.fold_left (fun init (x,y) -> init + array_test x y) 0 (Array.combine pattern.edges_opt cube.edges)
  + Array.fold_left (fun init (x,y) -> init + array_test x y) 0 (Array.combine pattern.corners_opt cube.corners)

let solve (cube:cube) (pattern:matching_cube) : cube*sequence =

  let rec beam_search : (cube*sequence) list -> cube*sequence = function
      [] -> failwith "not found"
    | k_best ->
      if cube_matching pattern (fst (List.hd k_best)) then let cube,seq = List.hd k_best in cube,List.rev seq else

      let get_next (c,seq:cube*sequence) : (cube*sequence) list =
        [F ; F' ; B ; B' ; U ; U' ; D ; D' ; L ; L' ; R ; R']
        |> List.map (fun m -> moved_cube c m, m::seq)
      in

      let nexts = List.concat (List.map get_next k_best) in
      let k_best_nexts = List.sort (fun x y -> compare (cube_matching_range pattern (fst x)) (cube_matching_range pattern (fst y)) ) nexts in
      beam_search (List.filteri (fun i _ -> i < k) k_best_nexts)

  in
  beam_search [cube,[]]

(* let a_star (cube:cube) (pattern:matching_cube) : sequence = *)
(*   (\** [a_star c p] returns a minimum-size list of moves that produce the pattern p from the cube c. *\) *)

(*   let rec search (c:cube) (seq:sequence) = *)
(*     [F ; F' ; B ; B' ; U ; U' ; D ; D' ; L ; L' ; R ; R'] *)
(*     |> List.map (moved_cube c) *)
(*     |> List.map (cube_matching_range) *)


(* ZIPER POUR GARDER L'INFO DU MOUVEMENT EFFECTUÉ *)


(* let match_face f pattern : bool = (\* [|[|false;White;false|];[|White;false;White|];[|false;White;false|]|] *\) *)
(*   Array.for_all2 (fun t1 t2 -> Array.for_all2 (fun m1 m2 -> m1 = m2 || m2 = false) t1 t2) f pattern *)

(* let do_white_cross cube = *)
(*   (\* croix blanche pas forcément bien placée. *\) *)
(*   let eval face = *)
(*     [face.(0).(1);face.(1).(0);face.(0).(2);face.(2).(0)] *)
(*     |> List.map (function White -> 1 | _ -> 0) *)
(*     |> List.fold_left (+) 0 *)
(*   and k = 12*12*12 in *)

(*   let beam_search cube mouvements = *)
(*     let face = Simulator.get_face cube White in () *)
(*     if eval face = 4 then cube *)
(*     else let suivants = Array.make *)
(*   in *)
(*   () *)
