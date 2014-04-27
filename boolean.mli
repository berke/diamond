(* Boolean *)
(* $Id: boolean.mli,v 1.3 2002/10/11 12:05:21 durak Exp $ *)

type 'a t =
  Atom of 'a
| And of 'a t * 'a t
| Or of 'a t * 'a t
| Xor of 'a t * 'a t
| Not of 'a t
| True
| False
type 'a possibility = Always | Maybe of 'a | Never
type 'a evaluation = No | Depends of 'a | Yes

val eval : ('a -> bool) -> 'a t -> bool
val to_string : ('a -> string) -> 'a t -> string
val iter : ('a -> unit) -> 'a t -> unit
val map : ('a -> 'b) -> 'a t -> 'b t
val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val simplify : ('a -> 'b possibility) -> 'a t -> 'b t

