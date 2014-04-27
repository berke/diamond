(* Catalog *)
(* $Id: catalog.ml,v 1.6 2002/10/15 14:35:42 durak Exp $ *)

module Make(S:Map.OrderedType) =
  struct
	module SM = Map.Make(S)
	module IM = Map.Make(struct type t = int let compare = compare end)
		
	type 'a t = {
		mutable serial : int;
		mutable index : int SM.t;
		mutable table : (S.t option * 'a) IM.t
	  }

	exception Registered_twice
		
	let create () = {
	  serial = 0;
	  index = SM.empty;
	  table = IM.empty
	}
		
	let lookup c id = SM.find id c.index
		
	let find c i = let (_,x) = IM.find i c.table in x

	let name_of c i = let (x,_) = IM.find i c.table in x
  
	let dump c oc f g = IM.iter
		(fun i (x,y) -> Printf.fprintf oc "    %-5d %s %a\n" i
			(match x with
			  None -> "(anonymous)"
			| Some(x') -> f x')
			g y) c.table
		
	let register c s t =
	  try
		SM.find s c.index
	  with
		Not_found ->
		  c.index <- SM.add s c.serial c.index;
		  c.table <- IM.add c.serial (Some s,t) c.table;
		  c.serial <- 1 + c.serial;
		  c.serial - 1
			
	let cardinality c = c.serial
		
	let register_anonymous_fixpoint c f =
	  let s = c.serial in
	  c.serial <- 1 + s;
	  let y = f s in
	  c.table <- IM.add s (None,y) c.table;
	  s
		
	let register_anonymous_fixpoints c f m =
	  let s0 = c.serial in
	  let results = Array.init m (fun i -> f i (s0 +  i)) in
	  c.serial <- c.serial + m;
	  for i = 0 to m - 1 do
		c.table <- IM.add (s0 + i) (None,results.(i)) c.table
	  done;
	  s0
		
	let register_fixpoints c a f =
	  let m = Array.length a in
	  let s0 = c.serial in
	  c.serial <- c.serial + m;
	  for i = 0 to m - 1 do
		let (id,x) = a.(i) in
		c.index <- SM.add id (s0 + i) c.index;
		c.table <- IM.add (s0 + i) (Some id,x) c.table
	  done;
	  let b = Array.init m (fun i -> f i (s0 +  i)) in
	  for i = 0 to m - 1 do
		let (id,_) = a.(i) in
		c.table <- IM.add (s0 + i) (Some id,b.(i)) c.table
	  done;
	  s0
		
	let register_anonymous c t =
	  c.table <- IM.add c.serial (None,t) c.table;
	  c.serial <- 1 + c.serial;
	  c.serial - 1
		
	let register_once c s t =
	  if SM.mem s c.index then
		raise Registered_twice
	  else
		begin
		  c.index <- SM.add s c.serial c.index;
		  c.table <- IM.add c.serial (Some s,t) c.table;
		  c.serial <- 1 + c.serial;
		  c.serial - 1
		end
		  
	let register_overriding c s t =
	  c.index <- SM.add s c.serial c.index;
	  c.table <- IM.add c.serial (Some s,t) c.table;
	  c.serial <- 1 + c.serial;
	  c.serial - 1
  end
