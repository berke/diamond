(* Label *)
(* $Id: label.ml,v 1.1 2002/10/11 12:05:21 durak Exp $ *)

type t = int
let counter = ref 0
let next () =
  incr counter;
  !counter - 1
let compare = compare
let to_string l = Printf.sprintf "l%d" l

