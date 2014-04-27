(* Label *)
(* $Id: label.mli,v 1.1 2002/10/11 12:05:21 durak Exp $ *)

type t
val next : unit -> t
val compare : t -> t -> int
val to_string : t -> string
