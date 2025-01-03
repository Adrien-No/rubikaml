(* on applique des mouvements jusqu'à obtenir le cube destination à partir du cube source *)
(* de manière équivalente, on pourra partir d'une suite de mouvenements (à appliqué à un cube résolu) *)

(* TODO foncteur à partir d'une égalité *)
(* TODO éliminer les chemins triviaux *)

(* to optimize the research we don't do the opposite alg of the one we just realized *)

let inv = function
  | "F" -> "F'" | "F'" -> "F"
  | "B" -> "B'" | "B'" -> "B"
  | "U" -> "U'" | "U'" -> "U"
  | "D" -> "D'" | "D'" -> "D"
  | "L" -> "L'" | "L'" -> "L"
  | "R" -> "R'" | "R'" -> "R"
  | s -> s

let move_of_string = let open Simulator in
  function
  | "F" -> f | "F'" -> f'
  | "B" -> b | "B'" -> b'
  | "U" -> u | "U'" -> u'
  | "D" -> d | "D'" -> d'
  | "L" -> l | "L'" -> l'
  | "R" -> r | "R'" -> r'
  | "U2" -> fun c -> c |> u |> u
  | s -> failwith ("can't get a move from " ^ s)

let moves prev = let open Simulator in
  ["F", f; "F'", f'; "B", b; "B'", b'; "U", u; "U'", u'; "D", d; "D'", d'; "L", l; "L'", l'; "R", r; "R'", r']
  |> List.filter (fun (name, _) -> name<>(inv prev))

let path_to_string = String.concat " "

(* NOTE on fait bien un dfs ici qui ne privilegie pas les chemins les plus courts, juste trouve un chemin *)
(** Trouve de manière exhaustive un chemin (suite de mouvements) permettant d'aller du premier cube au deuxième (selon l'égalité en paramètre) *)

(* let timeout = 2. *)
(* let path c_src c_dst = *)
(*   try *)
(*     let t0 = Unix.time() in *)
(*     let cache = Hashtbl.create 16 in (\* if we wanted to get the shortest path we could store the length in cache and update iff the len is lower /!\ can have in some cases bad complexity (decrementing n times a counter) *\) *)
(*     let rec dfs c path = *)
(*       if Unix.time() -. t0 > timeout then raise Not_found *)
(*       else if c = c_dst then raise (Found path) *)
(*       else if not (Hashtbl.mem cache c) then begin *)
(*         Hashtbl.add cache c (); *)
(*         (Simulator.to_string c) |> Printf.printf "%s\n%!"; *)
(*         (\* opti : iterer que sur les cubes ≠ (avec tests d'egalite / sort_uniq) *\) *)
(*         let nexts = List.map (fun (move_name, move) -> move c, move_name::path) (match path with [] -> moves "" | h::_ -> moves h ) in (\* est-ce pas couteux de commencer la fonction dfs pour tous ? meme ceux deja dans le cache ? *\) *)
(*         List.iter (fun (c, path) -> dfs c path) nexts *)
(*       end *)
(*     in *)
(*     dfs c_src []; raise Not_found *)
(*   with Found l -> l *)

(* avec bfs *)
exception Found of string list
let pure_path eq c_src c_dst =
  (* let cache = Hashtbl.create 16 in *)
  let q = Queue.create() in
  Queue.add (c_src, []) q;
  try
    while true do
      let (c, path) = Queue.pop q in
      if eq c c_dst then raise (Found path)
      else begin
        (* nexts *)
        List.iter (fun (move_name, move) ->
            let c = move c in
            Queue.add (c, move_name::path) q
            (* if not (Hashtbl.mem cache c) then ( *) (* for now we dont came back often enought in a same configuration *)
            (*   (\* Hashtbl.replace cache c (); *\) *)
          ) (match path with [] -> moves "" | h::_ -> moves h );
      end
    done
  with Found l -> l
