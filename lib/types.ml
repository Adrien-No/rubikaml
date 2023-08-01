(* ================================ CUBE ================================= *)

type color = White | Blue | Red | Green | Orange | Yellow
type center = color
type edge = color*color
type corner = color*color*color

type cubie = Center of center | Edge of edge | Corner of corner

type cube =
  (* A classical reprensentation of a rubik's cube. *)
  {
    centers : center array;
    edges : edge array; (* The face "in front of us" is always the Green one, on the top the White one. We denote edges by "notation priority" (see bellow) and (when we are on face) in CLOCKWISE-DIRECTION (beginning on top left of face and is around the center)*)
    corners : corner array; (* orientation priority, then clockwise-direction *)
  }

type matching_cube =
  (* Used to compare and match a specific pattern-goal we want to obtain. *)
  (*  If None then the pattern is matched. *)
  {
    centers_opt : center option array;
    edges_opt : edge option array;
    corners_opt : corner option array;
  }

type cubies =
  {
    t1 : cubie array;
    t2 : cubie array;
    t3 : cubie array;
  }

type matching_cubies =
  {
    t1_m : cubie option array;
    t2_m : cubie option array;
    t3_m : cubie option array;
  }

type face = color array array (* 3x3 *)


(* ================================ MOVES ================================ *)

type move = F | F' | B | B' | U | U' | D | D' | L | L' | R | R' (*| false*) (* false is used to recognize a pattern on a face *)
type sequence = move list
