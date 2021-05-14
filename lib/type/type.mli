type t

val t_of_int : int -> t

val t_of_list : int list -> t

val zero : t

val one : t

val compare : t -> t -> int

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t

val mod_big : t -> t -> t
