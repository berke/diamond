(* Loc *)
(* $Id: loc.mli,v 1.1.1.1 2002/09/30 10:09:37 durak Exp $ *)

exception Parse_error of int * int * string
type t = int * int

