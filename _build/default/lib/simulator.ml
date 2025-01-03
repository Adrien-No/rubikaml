(* pour faire un solveur optimisé : https://kociemba.org/cube.htm *)

(* ################### *)
(* #### NOTATIONS #### *)
(* ################### *)


(* cube complet *)

(* orientation des cubies : priorité des couleurs : B G Y W O G *)
(* position des cubies : on nomme un cubie selon les faces dont il fait parti  *)

(*     YYY *)
(*     YYY *)
(*     YYY *)

(* OOO BBB RRR GGG *)
(* OOO BBB RRR GGG *)
(* OOO BBB RRR GGG *)

(*     WWW *)
(*     WWW *)
(*     WWW *)

type color = W | Y | R | O | B | G (* White | Yellow | Red | Orange | Blue | Green *)

let string_of_color short = function
  | W -> if short then "⬜" else "White"
  | Y -> if short then "🟨" else "Yellow"
  | R -> if short then "🟥" else "Red"
  | O -> if short then "🟧" else "Orange"
  | B -> if short then "🟦" else "Blue"
  | G -> if short then "🟩" else "Green"

type corner = { fst : color;  snd : color;  trd : color}
(* let string_of_corner short (c1, c2, c3) = if short then Printf.sprintf "(%s, %s, %s)" (string_of_color true c1) (string_of_color true c2) (string_of_color true c3) *)
(*       else  *)

type edge = { fst : color;  snd : color}

type cube = { (* la première lettre correspond à la face, et la ou les suivantes à la position sur la face *)
  ful : corner;
  fur : corner;
  fdl : corner;
  fdr : corner;
  bul : corner;
  bur : corner;
  bdl : corner;
  bdr : corner;

  fu : edge;
  fl : edge;
  fr : edge;
  fd : edge;
  bu : edge;
  bl : edge;
  br : edge;
  bd : edge;

  ul : edge;
  ur : edge;
  dl : edge;
  dr : edge;
}

type t = cube

let create() : t = (* since cubes are finally unmutable, we could use a single solved cube val create : t  *)
  let three_to_corner (c1, c2, c3) = {fst=c1; snd=c2; trd=c3}
  and two_to_edge (c1, c2) = {fst=c1; snd=c2} in
  {
    ful = three_to_corner (B, Y, O);
    fur = three_to_corner (B, Y, R);
    fdl = three_to_corner (B, W, O);
    fdr = three_to_corner (B, W, R);
    bul = three_to_corner (G, Y, O);
    bur = three_to_corner (G, Y, R);
    bdl = three_to_corner (G, W, O);
    bdr = three_to_corner (G, W, R);

    fu = two_to_edge (B, Y);
    fl = two_to_edge (B, O);
    fr = two_to_edge (B, R);
    fd = two_to_edge (B, W);
    bu = two_to_edge (G, Y);
    bl = two_to_edge (G, O);
    br = two_to_edge (G, R);
    bd = two_to_edge (G, W);

    ul = two_to_edge (Y, O);
    ur = two_to_edge (Y, R);
    dl = two_to_edge (W, O);
    dr = two_to_edge (W, R);
  }

let to_string (c: cube) : string = (* can generalize to "to_facets" if needed *) (* NOTE could have used topology to transform cube in 2d patreon ? *)
  let f = string_of_color true in
  let sep = "" in
  (* let sepline = "\n" in *)
  "      "                                   ^f c.bul.snd^f c.bu.snd^f c.bur.snd^
  "\n      "                                 ^f c.ul.fst ^f Y       ^f c.ur.fst ^
  "\n      "                                 ^f c.ful.snd^f c.fu.snd^f c.fur.snd^
  "\n"^f c.bul.trd^f c.ul.snd^f c.ful.trd^sep^f c.ful.fst^f c.fu.fst^f c.fur.fst^sep^f c.fur.trd^f c.ur.snd^f c.bur.trd^sep^f c.bur.fst^f c.bu.fst^f c.bul.fst^
  "\n"^f c.bl.snd ^f O       ^f c.fl.snd ^sep^f c.fl.fst ^f B       ^f c.fr.fst ^sep^f c.fr.snd ^f R       ^f c.br.snd ^sep^f c.br.fst ^f G       ^f c.bl.fst ^
  "\n"^f c.bdl.trd^f c.dl.snd^f c.fdl.trd^sep^f c.fdl.fst^f c.fd.fst^f c.fdr.fst^sep^f c.fdr.trd^f c.dr.snd^f c.bdr.trd^sep^f c.bdr.fst^f c.bd.fst^f c.bdl.fst^
  "\n      "                                 ^f c.fdl.snd^f c.fd.snd^f c.fdr.snd^
  "\n      "                                 ^f c.dl.fst ^f W       ^f c.dr.fst ^
  "\n      "                                 ^f c.bdl.snd^f c.bd.snd^f c.bdr.snd

(* print enought facets to recognize cmll case *)
let to_cmll (c : cube) : string =
  let f = string_of_color true in
  let sep = " " in
                        f c.bul.fst^f c.bu.fst^f c.bur.fst^
  "\n"^f c.bul.trd^sep ^f c.bul.snd^f c.bu.snd^f c.bur.snd^ sep^f c.bur.trd^
  "\n"^f c.ul.snd ^sep ^f c.ul.fst ^f Y       ^f c.ur.fst ^ sep^f c.ur.snd ^
  "\n"^f c.ful.trd^sep ^f c.ful.snd^f c.fu.snd^f c.fur.snd^ sep^f c.fur.trd^
  "\n"^                 f c.ful.fst^f c.fu.fst^f c.fur.fst

  (*                      f c.bul.fst^f c.bu.fst^f c.bul.fst^ *)
  (* "\n"^f c.bul.trd^sep^f c.bul.snd^f c.bu.snd^f c.bur.snd^sep^f c.bur.trd^ *)
  (* "\n"^f c.ul.snd ^sep^f c.ul.fst ^f Y       ^f c.ur.fst ^sep^f c.ur.snd ^ *)
  (* "\n"^f c.ful.trd^sep^f c.ful.snd^f c.fu.snd^f c.fur.snd^sep^f c.fur.trd^ *)
  (* "\n"^                f c.ful.fst^f c.fu.fst^f c.fur.fst *)

let cache = Hashtbl.create 16
let id = ref 0
(* permet d'attribuer un entier unique à chaque permutation utilisée du cube *)
(* utilisée nottemment par l'interface de graphe *)
let to_int (c : t) : int =
  match Hashtbl.find_opt cache c with
  | None -> incr id; Hashtbl.add cache c !id; !id
  | Some i -> i
(* let fold_corners (c : cube) (f: 'a -> (corner * string) -> 'a) (init: 'a) : 'a = *)
(*   List.fold_left f init [(c.ful, "ful"); (c.fur, "fur"); (c.fdl, "fdl"); (c.fdr, "fdr"); (c.bul, "bul"); (c.bur, "bur"); (c.bdl, "bdl"); (c.bdr, "bdr")] *)

(* let fold_edges c f init = *)
(*   List.fold_left f init [(c.fu, "fu"); (c.fl, "fl"); (c.fr, "fr"); (c.fd, "fd"); (c.bu, "bu"); (c.bl, "bl"); (c.br, "br"); (c.bd, "bd"); (c.ul, "ul"); (c.ur, "ur"); (c.dl, "dl"); (c.dr, "dr")] *)

(* let fold_cube c f f' init init' = *)
(*   fold_corners c f init, fold_edges c f' init' *)

(* let fold2_corners (c1 : cube) (c2 : cube) (f: 'a -> (corner * string) -> 'a) (init: 'a) : 'a = *)
(*   List.fold_left f init [(c.ful, c2.ful"ful"); (c.fur, "fur"); (c.fdl, "fdl"); (c.fdr, "fdr"); (c.bul, "bul"); (c.bur, "bur"); (c.bdl, "bdl"); (c.bdr, "bdr")] *)

(* let fold2_edges c f init = *)
(*   List.fold_left f init [(c.fu, "fu"); (c.fl, "fl"); (c.fr, "fr"); (c.fd, "fd"); (c.bu, "bu"); (c.bl, "bl"); (c.br, "br"); (c.bd, "bd"); (c.ul, "ul"); (c.ur, "ur"); (c.dl, "dl"); (c.dr, "dr")] *)

(* let fold2_cube c f f' init init' = *)
(*   fold_corners c f init, fold_edges c f' init' *)



(* let to_lists c = [c.ful; c.fur; c.fdl; c.fdr; c.bul; c.bur; c.bdl; c.bdr],  (\* NOTE use ppx ? https://github.com/janestreet/ppx_fields_conv *\) *)
(*                  [c.fu; c.fl; c.fr; c.fr; c.fd; c.bu; c.bl; c.br; c.bd; c.ul; c.ur; c.dl; c.dr] *)

(* (\* let wilder w = *\) *)

(* let (===) c c' ?(ful=false) ?(fur=false) ?(fdl=false) ?(fdr=false) ?() = *)
(*   let w_c, w_e = fst wilds, snd wilds in *)
(*   assert(w_c = List.sort_uniq compare w_c && w_e = List.sort_uniq compare w_e); *)
(*   let rec check_edges wilds c c' = *)

(* for best perfomances we will do several different "≡" instead of one general that has options *)
let (===) = (=)

(* NOTE we could also take in count Us variations *)
let cmll_eq c c' =
  c.ful=c'.ful&&c.fur=c'.fur&&c.fdl=c'.fdl&&c.fdr=c'.fdr&&c.bul=c'.bul&&c.bur=c'.bur&&c.bdl=c'.bdl&&c.bdr=c'.bdr&&
  c.fl =c'.fl &&c.fr =c'.fr &&c.bl =c'.bl &&c.br =c'.br &&c.dl =c'.dl &&c.dr =c'.dr

(* we check is two first blocks are made *)
let is_cmll (c : cube) =
  let c' = create() in
  c.fdl=c'.fdl&&c.fdr=c'.fdr&&c.bdl=c'.bdl&&c.bdr=c'.bdr&&
  c.fl =c'.fl &&c.fr =c'.fr &&c.bl =c'.bl &&c.br =c'.br &&c.dl =c'.dl &&c.dr =c'.dr

  let f c = (* NOTE mieux de modifier c ou bien de renvoyer un c' ? on prend la 2e option *)
    let flip {fst; snd; trd} = {fst; snd=trd; trd=snd} in
    {
      ful = c.fdl |> flip; (* changed *)
      fur = c.ful |> flip; (* changed *)
      fdl = c.fdr |> flip; (* changed *)
      fdr = c.fur |> flip; (* changed *)
      bul = c.bul;
      bur = c.bur;
      bdl = c.bdl;
      bdr = c.bdr;

      fu = c.fl; (* changed *)
      fl = c.fd; (* changed *)
      fr = c.fu; (* changed *)
      fd = c.fr; (* changed *)
      bu = c.bu;
      bl = c.bl;
      br = c.br;
      bd = c.bd;

      ul = c.ul;
      ur = c.ur;
      dl = c.dl;
      dr = c.dr;
    }

  let f' c =
    let flip {fst; snd; trd} = {fst; snd=trd; trd=snd} in
    {
      ful = c.fur |> flip; (* changed *)
      fur = c.fdr |> flip; (* changed *)
      fdl = c.ful |> flip; (* changed *)
      fdr = c.fdl |> flip; (* changed *)
      bul = c.bul;
      bur = c.bur;
      bdl = c.bdl;
      bdr = c.bdr;

      fu = c.fr; (* changed *)
      fl = c.fu; (* changed *)
      fr = c.fd; (* changed *)
      fd = c.fl; (* changed *)
      bu = c.bu;
      bl = c.bl;
      br = c.br;
      bd = c.bd;

      ul = c.ul;
      ur = c.ur;
      dl = c.dl;
      dr = c.dr;
    }

let b c =
    let flip {fst; snd; trd} = {fst; snd=trd; trd=snd} in
    {
      ful = c.ful;
      fur = c.fur;
      fdl = c.fdl;
      fdr = c.fdr;
      bul = c.bur |> flip; (* changed *)
      bur = c.bdr |> flip; (* changed *)
      bdl = c.bul |> flip; (* changed *)
      bdr = c.bdl |> flip; (* changed *)

      fu = c.fu;
      fl = c.fl;
      fr = c.fr;
      fd = c.fd;
      bu = c.br; (* changed *)
      bl = c.bu; (* changed *)
      br = c.bd; (* changed *)
      bd = c.bl; (* changed *)

      ul = c.ul;
      ur = c.ur;
      dl = c.dl;
      dr = c.dr;
    }

let b' c =
    let flip {fst; snd; trd} = {fst; snd=trd; trd=snd} in
    {
      ful = c.ful;
      fur = c.fur;
      fdl = c.fdl;
      fdr = c.fdr;
      bul = c.bdl |> flip; (* changed *)
      bur = c.bul |> flip; (* changed *)
      bdl = c.bdr |> flip; (* changed *)
      bdr = c.bur |> flip; (* changed *)

      fu = c.fu;
      fl = c.fl;
      fr = c.fr;
      fd = c.fd;
      bu = c.bl; (* changed *)
      bl = c.bd; (* changed *)
      br = c.bu; (* changed *)
      bd = c.br; (* changed *)

      ul = c.ul;
      ur = c.ur;
      dl = c.dl;
      dr = c.dr;
    }

  let u c =
    let flip3 {fst; snd; trd} = {fst=trd; snd=snd; trd=fst} in
    let flip2 {fst; snd} = {fst=snd; snd=fst} in
    {
      ful = c.fur |> flip3; (* changed *)
      fur = c.bur |> flip3; (* changed *)
      fdl = c.fdl;
      fdr = c.fdr;
      bul = c.ful |> flip3; (* changed *)
      bur = c.bul |> flip3; (* changed *)
      bdl = c.bdl;
      bdr = c.bdr;

      fu = c.ur |> flip2; (* changed *)
      fl = c.fl;
      fr = c.fr;
      fd = c.fd;
      bu = c.ul |> flip2; (* changed *)
      bl = c.bl;
      br = c.br;
      bd = c.bd;

      ul = c.fu |> flip2; (* changed *)
      ur = c.bu |> flip2; (* changed *)
      dl = c.dl;
      dr = c.dr;
    }

  let u' c =
    let flip3 {fst; snd; trd} = {fst=trd; snd=snd; trd=fst} in
    let flip2 {fst; snd} = {fst=snd; snd=fst} in
    {
      ful = c.bul |> flip3; (* changed *)
      fur = c.ful |> flip3; (* changed *)
      fdl = c.fdl;
      fdr = c.fdr;
      bul = c.bur |> flip3; (* changed *)
      bur = c.fur |> flip3; (* changed *)
      bdl = c.bdl;
      bdr = c.bdr;

      fu = c.ul |> flip2; (* changed *)
      fl = c.fl;
      fr = c.fr;
      fd = c.fd;
      bu = c.ur |> flip2; (* changed *)
      bl = c.bl;
      br = c.br;
      bd = c.bd;

      ul = c.bu |> flip2; (* changed *)
      ur = c.fu |> flip2; (* changed *)
      dl = c.dl;
      dr = c.dr;
    }

let d c =
    let flip3 {fst; snd; trd} = {fst=trd; snd=snd; trd=fst} in
    let flip2 {fst; snd} = {fst=snd; snd=fst} in
    {
      ful = c.ful;
      fur = c.fur;
      fdl = c.bdl |> flip3; (* changed *)
      fdr = c.fdl |> flip3; (* changed *)
      bul = c.bul;
      bur = c.bur;
      bdl = c.bdr |> flip3; (* changed *)
      bdr = c.fdr |> flip3; (* changed *)

      fu = c.fu;
      fl = c.fl;
      fr = c.fr;
      fd = c.dl |> flip2; (* changed *)
      bu = c.bu;
      bl = c.bl;
      br = c.br;
      bd = c.dr |> flip2; (* changed *)

      ul = c.ul;
      ur = c.ur;
      dl = c.bd |> flip2; (* changed *)
      dr = c.fd |> flip2; (* changed *)
    }

let d' c =
    let flip3 {fst; snd; trd} = {fst=trd; snd=snd; trd=fst} in
    let flip2 {fst; snd} = {fst=snd; snd=fst} in
    {
      ful = c.ful;
      fur = c.fur;
      fdl = c.fdr |> flip3; (* changed *)
      fdr = c.bdr |> flip3; (* changed *)
      bul = c.bul;
      bur = c.bur;
      bdl = c.fdl |> flip3; (* changed *)
      bdr = c.bdl |> flip3; (* changed *)

      fu = c.fu;
      fl = c.fl;
      fr = c.fr;
      fd = c.dr |> flip2; (* changed *)
      bu = c.bu;
      bl = c.bl;
      br = c.br;
      bd = c.dl |> flip2; (* changed *)

      ul = c.ul;
      ur = c.ur;
      dl = c.fd |> flip2; (* changed *)
      dr = c.bd |> flip2; (* changed *)
    }

let l c =
    let flip3 {fst; snd; trd} = {fst=snd; snd=fst; trd} in
    {
      ful = c.bul |> flip3; (* changed *)
      fur = c.fur;
      fdl = c.ful |> flip3; (* changed *)
      fdr = c.fdr;
      bul = c.bdl |> flip3; (* changed *)
      bur = c.bur;
      bdl = c.fdl |> flip3; (* changed *)
      bdr = c.bdr;

      fu = c.fu;
      fl = c.ul; (* changed *)
      fr = c.fr;
      fd = c.fd;
      bu = c.bu;
      bl = c.dl; (* changed *)
      br = c.br;
      bd = c.bd; (* changed *)

      ul = c.bl; (* changed *)
      ur = c.ur;
      dl = c.fl; (* changed *)
      dr = c.dr;
    }

let l' c =
    let flip3 {fst; snd; trd} = {fst=snd; snd=fst; trd} in
    {
      ful = c.fdl |> flip3; (* changed *)
      fur = c.fur;
      fdl = c.bdl |> flip3; (* changed *)
      fdr = c.fdr;
      bul = c.ful |> flip3; (* changed *)
      bur = c.bur;
      bdl = c.bul |> flip3; (* changed *)
      bdr = c.bdr;

      fu = c.fu;
      fl = c.dl; (* changed *)
      fr = c.fr;
      fd = c.fd;
      bu = c.bu;
      bl = c.ul; (* changed *)
      br = c.br;
      bd = c.bd;

      ul = c.fl; (* changed *)
      ur = c.ur;
      dl = c.bl; (* changed *)
      dr = c.dr;
    }

let r c =
    let flip3 {fst; snd; trd} = {fst=snd; snd=fst; trd} in
    {
      ful = c.ful;
      fur = c.fdr |> flip3; (* changed *)
      fdl = c.fdl;
      fdr = c.bdr |> flip3; (* changed *)
      bul = c.bul;
      bur = c.fur |> flip3; (* changed *)
      bdl = c.bdl;
      bdr = c.bur |> flip3; (* changed *)

      fu = c.fu;
      fl = c.fl;
      fr = c.dr; (* changed *)
      fd = c.fd;
      bu = c.bu;
      bl = c.bl;
      br = c.ur; (* changed *)
      bd = c.bd;

      ul = c.ul;
      ur = c.fr; (* changed *)
      dl = c.dl;
      dr = c.br; (* changed *)
    }

let r' c =
    let flip3 {fst; snd; trd} = {fst=snd; snd=fst; trd} in
    {
      ful = c.ful;
      fur = c.bur |> flip3; (* changed *)
      fdl = c.fdl;
      fdr = c.fur |> flip3; (* changed *)
      bul = c.bul;
      bur = c.bdr |> flip3; (* changed *)
      bdl = c.bdl;
      bdr = c.fdr |> flip3; (* changed *)

      fu = c.fu;
      fl = c.fl;
      fr = c.ur; (* changed *)
      fd = c.fd;
      bu = c.bu;
      bl = c.bl;
      br = c.dr; (* changed *)
      bd = c.bd;

      ul = c.ul;
      ur = c.br; (* changed *)
      dl = c.dl;
      dr = c.fr; (* changed *)
    }

let wide_is_cmll (c : cube) =
  is_cmll c || is_cmll (u c) || is_cmll (c |> u |> u) || is_cmll (u' c)
