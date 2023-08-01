open Types

let center_nb = 6
let edges_nb = 12
let corners_nb = 8

(* get first/second/third of a 3-uplet *)
let f3u = function (x,_,_) -> x
let s3u = function (_,y,_) -> y
let t3u = function (_,_,z) -> z
let swap2 = function a,b -> b,a
let swapst3u = function (x,y,z) -> (x,z,y)
let swapft3u = function (x,y,z) -> (z,y,x)
let swapfs3u = function (x,y,z) -> (y,x,z)

let swap_corner = function x,y,z -> y,z,x
let swap_corner' = function x,y,z -> z,x,y

 (* orientation and notation priority (decreasing): Front, Back, Up, Down, Left, Right *)

(** [solved()] returns a fresh cube completed. *)
let solved () : cube =

  let cube = {
    centers = [|Green ; Blue ; White ; Yellow ; Orange ; Red|];
    edges = [|Green,White;Green,Red;Green,Yellow;Green,Orange ; Blue,White;Blue,Orange;Blue,Yellow;Blue,Red ; White,Red;White,Orange ; Yellow,Red;Yellow,Orange |];
    corners = [|Green,White,Orange;Green,White,Red;Green,Yellow,Red;Green,Yellow,Orange ; Blue,White,Red;Blue,White,Orange;Blue,Yellow,Orange;Blue,Yellow,Red |];
  }
  in cube

(** [fresh_pattern ()] returns a fresh matching_cube that match every cube for now. *)
let fresh_pattern () : matching_cube =
  {
    centers_opt = Array.make center_nb None;
    edges_opt = Array.make edges_nb None;
    corners_opt = Array.make corners_nb None;
  }

(** [copy c] returns a copy of [c] that is a fresh cube containing the same elements as c.*)
let copy (c:cube) : cube =
  {
    centers = Array.copy c.centers;
    edges = Array.copy c.edges;
    corners = Array.copy c.corners;
  }

(** each face is returned as if we were staring it by rotating the cube by : *)
let get_face c : color -> color array array = function
  (* - nothing for Green *)
  (* - y for Red *)
  (* - y' for Orange *)
  (* - y^2 for Blue *)
  (* - x for White *)
  (* - x' for Yellow *)
  (* Where x,y,z are movements specified for example in kungfuomanch.com, see "Andy Klise's Speedcubing Guide" *)
    Green -> [|[|f3u c.corners.(0) ;fst c.edges.(0)  ; f3u c.corners.(1)|];
               [|fst c.edges.(3)   ; c.centers.(0)   ; fst c.edges.(1)  |];
               [|f3u c.corners.(3) ; fst c.edges.(2) ; f3u c.corners.(2)|]
             |]
  | Blue  -> [|[|f3u c.corners.(4) ;fst c.edges.(4)  ; f3u c.corners.(5)|];
               [|fst c.edges.(7)   ; c.centers.(1)   ; fst c.edges.(5)  |];
               [|f3u c.corners.(7) ; fst c.edges.(6) ; f3u c.corners.(6)|]
             |]
  | White -> [|[|s3u c.corners.(5) ;snd c.edges.(4)  ; s3u c.corners.(4)|];
               [|fst c.edges.(9)  ; c.centers.(2)   ; fst c.edges.(8)  |];
               [|s3u c.corners.(0) ; snd c.edges.(0) ; s3u c.corners.(1)|]
             |]
  | Yellow-> [|[|s3u c.corners.(3) ;snd c.edges.(2)  ; s3u c.corners.(2)|];
               [|fst c.edges.(11)  ; c.centers.(3)   ; fst c.edges.(10) |];
               [|s3u c.corners.(6) ; snd c.edges.(6) ; s3u c.corners.(7)|]
             |]
  | Orange-> [|[|t3u c.corners.(5) ;snd c.edges.(9)  ; t3u c.corners.(0)|];
               [|snd c.edges.(5)   ; c.centers.(4)   ; snd c.edges.(3)  |];
               [|t3u c.corners.(6) ; snd c.edges.(11); t3u c.corners.(3)|]
             |]
  | Red   -> [|[|t3u c.corners.(1) ;snd c.edges.(8)  ; t3u c.corners.(4)|];
               [|snd c.edges.(1)   ; c.centers.(5)   ; snd c.edges.(7)  |];
               [|t3u c.corners.(2) ; snd c.edges.(10); t3u c.corners.(7)|]
             |]

(* ================================================================ printing ================================================================  *)

let print_char_array a = Array.iteri (fun i x -> Printf.printf "%c" x; if i < Array.length a -1 then Printf.printf " ") a; Printf.printf "\n"
let print_char_array_array a = Array.iter print_char_array a; Printf.printf "\n"

let charize_color = function
    Green -> 'G'
  | Blue -> 'B'
  | White -> 'W'
  | Yellow -> 'Y'
  | Orange -> 'O'
  | Red -> 'R'
let color_array = Array.map charize_color
let charize_color_array_array = Array.map color_array

let print_faces cube : unit =
  (* print a 2D representation of the cube *)
  let empty_face = Array.make_matrix 3 3 ' ' in
  let string_face c = (* prepare a face to be printed. In reality we first concat it to realise the 2D representation, not just printing a lonely face. *)
    c |> get_face cube |> charize_color_array_array
  in
  print_char_array_array (Array.map2 (fun x y -> Array.concat [x;[|' '|];y]) empty_face (string_face White));
  print_char_array_array (List.fold_left (fun init x -> Array.map2 (fun x y -> Array.concat [x;[|' '|];y]) init x) (string_face Orange) (List.map string_face [Green;Red;Blue]));
  print_char_array_array (Array.map2 (fun x y -> Array.concat [x;[|' '|];y]) empty_face (string_face Yellow))

let move_to_string : move -> string = function
    F -> "F"
  | F' -> "F'"
  | B -> "B"
  | B' -> "B'"
  | U -> "U"
  | U' -> "U'"
  | D -> "D"
  | D' -> "D'"
  | L -> "L"
  | L' -> "L'"
  | R -> "R"
  | R' -> "R'"
  (* | _ -> failwith "TypeError" *)

let print_sequence (s:sequence) : unit =
  List.iter (fun move -> move_to_string move |> Printf.printf "%s ") s; Printf.printf "\n"
(* ===========================================================================================================================================  *)

(** [move_cube c m] executes the move m on the cube c. *)
let move_cube (c:cube) : move->unit = function
    F -> let saved_edge,saved_corner = c.edges.(3), c.corners.(3) in for i = 3 downto 1 do c.corners.(i) <- swapst3u c.corners.(i-1); c.edges.(i) <- c.edges.(i-1) done; c.corners.(0) <- swapst3u saved_corner; c.edges.(0) <- saved_edge

  | F' -> let saved_edge,saved_corner = c.edges.(0), c.corners.(0) in for i = 0 to 2 do c.corners.(i) <- swapst3u c.corners.(i+1); c.edges.(i) <- c.edges.(i+1) done; c.corners.(3) <- swapst3u saved_corner; c.edges.(3) <- saved_edge

  | B -> let saved_edge,saved_corner = c.edges.(7), c.corners.(7) in for i = 7 downto 5 do c.corners.(i) <- swapst3u c.corners.(i-1); c.edges.(i) <- c.edges.(i-1) done; c.corners.(4) <- swapst3u saved_corner; c.edges.(4) <- saved_edge

  | B' -> let saved_edge,saved_corner = c.edges.(4), c.corners.(4) in for i = 4 to 6 do c.corners.(i) <- swapst3u c.corners.(i+1); c.edges.(i) <- c.edges.(i+1) done; c.corners.(7) <- swapst3u saved_corner; c.edges.(7) <- saved_edge

  | U -> let saved_corner,saved_edge = c.corners.(5),c.edges.(4) in c.corners.(5) <- swapft3u c.corners.(0) ; c.edges.(4) <- swap2 c.edges.(9); c.corners.(0) <- swapft3u c.corners.(1) ; c.edges.(9) <- swap2 c.edges.(0); c.corners.(1) <- swapft3u c.corners.(4) ; c.edges.(0) <- swap2 c.edges.(8); c.corners.(4) <- swapft3u saved_corner ; c.edges.(8) <- swap2 saved_edge

  | U' -> let saved_corner,saved_edge = c.corners.(4),c.edges.(8) in c.corners.(4) <- swapft3u c.corners.(1) ; c.edges.(8) <- swap2 c.edges.(0); c.corners.(1) <- swapft3u c.corners.(0) ; c.edges.(0) <- swap2 c.edges.(9); c.corners.(0) <- swapft3u c.corners.(5) ; c.edges.(9) <- swap2 c.edges.(4); c.corners.(5) <- swapft3u saved_corner ; c.edges.(4) <- swap2 saved_edge

  | D -> let saved_corner,saved_edge = c.corners.(6),c.edges.(11) in c.corners.(6) <- swapft3u c.corners.(7) ; c.edges.(11) <- swap2 c.edges.(6); c.corners.(7) <- swapft3u c.corners.(2) ; c.edges.(6) <- swap2 c.edges.(10); c.corners.(2) <- swapft3u c.corners.(3) ; c.edges.(10) <- swap2 c.edges.(2); c.corners.(3) <- swapft3u saved_corner ; c.edges.(2) <- swap2 saved_edge

  | D' -> let saved_corner,saved_edge = c.corners.(3),c.edges.(2) in c.corners.(3) <- swapft3u c.corners.(2) ; c.edges.(2) <- swap2 c.edges.(10); c.corners.(2) <- swapft3u c.corners.(7) ; c.edges.(10) <- swap2 c.edges.(6); c.corners.(7) <- swapft3u c.corners.(6) ; c.edges.(6) <- swap2 c.edges.(11); c.corners.(6) <- swapft3u saved_corner ; c.edges.(11) <- swap2 saved_edge

  | L -> let saved_corner,saved_edge = c.corners.(6),c.edges.(11) in c.corners.(6) <- swapfs3u c.corners.(3) ; c.edges.(11) <- c.edges.(3); c.corners.(3) <- swapfs3u c.corners.(0) ; c.edges.(3) <- c.edges.(9); c.corners.(0) <- swapfs3u c.corners.(5) ; c.edges.(9) <- c.edges.(5); c.corners.(5) <- swapfs3u saved_corner ; c.edges.(5) <- saved_edge

  | L' -> let saved_corner,saved_edge = c.corners.(5),c.edges.(5) in c.corners.(5) <- swapfs3u c.corners.(0) ; c.edges.(5) <- c.edges.(9); c.corners.(0) <- swapfs3u c.corners.(3) ; c.edges.(9) <- c.edges.(3); c.corners.(3) <- swapfs3u c.corners.(6) ; c.edges.(3) <- c.edges.(11); c.corners.(6) <- swapfs3u saved_corner ; c.edges.(11) <- saved_edge

  | R -> let saved_corner,saved_edge = c.corners.(1),c.edges.(1) in c.corners.(1) <- swapfs3u c.corners.(2) ; c.edges.(1) <- c.edges.(10);c.corners.(2) <- swapfs3u c.corners.(7) ; c.edges.(10) <- c.edges.(7);c.corners.(7) <- swapfs3u c.corners.(4) ; c.edges.(7) <- c.edges.(8);c.corners.(4) <- swapfs3u saved_corner ; c.edges.(8) <- saved_edge

  | R' -> let saved_corner,saved_edge = c.corners.(4),c.edges.(8) in c.corners.(4) <- swapfs3u c.corners.(7) ; c.edges.(8) <- c.edges.(7);c.corners.(7) <- swapfs3u c.corners.(2) ; c.edges.(7) <- c.edges.(10);c.corners.(2) <- swapfs3u c.corners.(1) ; c.edges.(10) <- c.edges.(1);c.corners.(1) <- swapfs3u saved_corner ; c.edges.(1) <- saved_edge

  (* | _ -> failwith "TypeError" *)

(** [moved_cube c m] returns a copy of c with the move m executed. *)
let moved_cube (c:cube) (m:move) : cube =
  let c' = copy c in
  move_cube c' m;
  c'


(* ================ sorting colors ================  *)

(** Same as compare but for colors, using the following descending priority : Green / Blue / White / Yellow / Orange / Red. *)
let compare_color c1 c2 =
  (* Note that it doesn't exists cubies with color-distance <= 1.*)
  let weigthed = function
      Green -> 6
    | Blue -> 5
    | White -> 4
    | Yellow -> 3
    | Orange -> 2
    | Red -> 1
  in
  compare (weigthed c1) (weigthed c2)

(* let normalize_cubie : cubie -> cubie = function *)
(*   (\** sort colors of a cubie depending of representation priority : (descending) Green / Blue / White / Yellow / Orange / Red *\) *)
(*     Center c -> Center c *)
(*   | Edge c1,c2 -> Edge  *)

(* let compare_cubies c1 c2 = function *)

let cubify_center = fun c -> Center c
let cubify_center_opt : center option -> cubie option = function Some c -> Some (Center c) | _ -> None (* failwith "TypeError" *)
let cubify_edge = fun e -> Edge e
let cubify_edge_opt : edge option -> cubie option = function Some c -> Some (Edge c) | _ -> None (* failwith "TypeError" *)
let cubify_corner = fun c -> Corner c
let cubify_corner_opt : corner option -> cubie option = function Some c -> Some (Corner c) | _ -> None (* failwith "TypeError" *)

let cubify_cube (cube:cube) : cubies =
  {
    t1 = Array.map cubify_center cube.centers;
    t2 = Array.map cubify_edge cube.edges;
    t3 = Array.map cubify_corner cube.corners;
  }

let cubify_matching_cube (mcube:matching_cube) : matching_cubies =
  {
    t1_m = (Array.map cubify_center_opt mcube.centers_opt:cubie option array);
    t2_m = Array.map cubify_edge_opt mcube.edges_opt;
    t3_m = (Array.map cubify_corner_opt mcube.corners_opt:cubie option array);
  }
let compare_cubie : cubie * cubie -> bool =
  let normalize_cubie = function
      Center c -> Center c
    | Edge c -> let sorted = List.sort compare_color [fst c; snd c] in
      Edge (List.nth sorted 1, List.nth sorted 0)
    | Corner c -> let sorted = List.sort compare_color [f3u c;s3u c; t3u c] in
      Corner(List.nth sorted 2, List.nth sorted 1, List.nth sorted 0)
  in
  function
    c1,c2 -> normalize_cubie c1 = normalize_cubie c2

(* ================================================== *)

(* ==========================================================================================================================================  *)


let exec_sequence (c:cube) : sequence -> unit =
  List.iter (move_cube c)
