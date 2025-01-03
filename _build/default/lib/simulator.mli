type t
val create : unit -> t
val to_string : t -> string
val to_cmll : t -> string
val to_int : t -> int
val (===) : t -> t -> bool

val is_cmll : t -> bool
val wide_is_cmll : t -> bool (* doesn't count Us symetries *)

val f  : t -> t
val f' : t -> t
val b  : t -> t
val b' : t -> t
val u  : t -> t
val u' : t -> t
val d  : t -> t
val d' : t -> t
val l  : t -> t
val l' : t -> t
val r  : t -> t
val r' : t -> t
