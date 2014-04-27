(* Catalog *)
(* $Id: catalog.mli,v 1.3 2002/10/15 12:21:11 durak Exp $ *)

module Make : functor (S:Map.OrderedType) ->
  sig
	type 'a t
	exception Registered_twice
	val create : unit -> 'a t
	val find : 'a t -> int -> 'a
	val lookup : 'a t -> S.t -> int
	val dump : 'a t -> out_channel -> (S.t -> string) -> (out_channel -> 'a -> unit) -> unit
	val register : 'a t -> S.t -> 'a -> int
	val cardinality : 'a t -> int
	val name_of : 'a t -> int -> S.t option
	val register_anonymous_fixpoint : 'a t -> (int -> 'a) -> int
	val register_anonymous_fixpoints : 'a t -> (int -> int -> 'a) -> int -> int
	val register_fixpoints : 'a t -> (S.t * 'a) array -> (int -> int -> 'a) -> int
	val register_anonymous : 'a t -> 'a -> int
	val register_once : 'a t -> S.t -> 'a -> int
	val register_overriding : 'a t -> S.t -> 'a -> int
  end
