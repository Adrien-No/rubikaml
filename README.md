### Introduction

The aim of this project is to help convincing us why speed-cubing algorithms for rubik's cube (of size 3x3) works.
By doing so, we could learn more intuitively these algorithms.

### Functionalities
- A quite naive but also quite safe **simulator**, which allow to move a cube (for best performances we can see this really nice page : https://kociemba.org/cube.htm )
- A **solver**, which find the shortest path (in term of moves) between two configuration of the rubik's cube (/!\ currently not powerful enough to find algorithms of size > 10 moves in reasonable time)
- Graph interface to draw Cayley graph of subgroups of rubik's cube (we currently focused on CMLL step of "Roux" speed cubing method)


### Improvement

#### short-term 
- /!\ correct print of cmll

- add smartly edges between CMLL configurations to improve
- add algorithms for missing CMLLs (H_brow, Pi_cols, U_x, T_frow, T_brow, T_cols) which fastest agls require fw moves than aren't yet implemented.
- finally, contract paths (e.g u->r->u'->r' became just --u r u' r'-->)

#### long-term
- find new ways of representing algorithms (make algebraic properties visible : conjugation, action, commutator .., show opposite algs ) 
