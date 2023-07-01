type color = White | Blue | Red | Green | Orange | Yellow
type center = color
type edge = color*color
type corner = color*color*color

type move = F | F' | B | B' | U | U' | D | D' | L | L' | R | R'

type cube =
  {
    centers : center array;
    edges : edge array; (* The face "in front of us" is always the Green one, on the top the White one. We denote edges by "notation priority" (see bellow) and (when we are on face) in CLOCKWISE-DIRECTION (beginning on top left of face and is around the center)*)
    corners : corner array; (* orientation priority, then clockwise-direction *)
  }

type sequence = move list
