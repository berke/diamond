(* Transform *)
(* $Id: transform.ml,v 1.4 2002/10/16 13:52:21 durak Exp $ *)

open Ast
open Intermediate

module IS = Set.Make(struct type t = int let compare = compare end)
module SM = Map.Make(String)
module C = Catalog.Make(String)

let rec args_of_st e (s : IS.t) = function
	`Statement(_,st) -> args_of_st e s st
  | `Sequence(l) -> List.fold_left (args_of_st e) s l
  | `While(c,st) -> args_of_st e (args_of_c e s c) st
  | `Op(o) -> args_of_op e s o
  | `Let(_,st) -> args_of_st e s st
  | `Apply(id,l) -> List.fold_left (args_of_xp e) s l
  | `If(c,st1,st2) -> args_of_st e (args_of_st e (args_of_c e s c) st1) st2
and args_of_op e (s : IS.t) = function
	(Push(xp)|Mark(_,xp)|Output(xp)) -> args_of_xp e s xp
  | (Pop|Left|Right|Accept|Reject|Halt) -> s
and args_of_c e s = Boolean.fold (args_of_t e) s
and args_of_t e s = function
	(`Empty|`Eof|`Home) -> s
  | (`Marked(_,xp)|`See(xp)|`Top(xp)) -> args_of_xp e s xp
  | `Relation(_,xp1,xp2) -> args_of_xp e (args_of_xp e s xp1) xp2
and args_of_xp e s = function
	Expression(_,xp) -> args_of_xp e s xp
  | String(_) -> s
  | Var(id) ->
	  try
		match SM.find id e with
		  Constant(_) -> s
		| Argument(i) -> IS.add i s
	  with
		Not_found -> raise (Undefined_variable id)
			
let rec resolve_value values e = function
	Expression(_,xp) -> resolve_value values e xp
  | String(s) -> Constant(C.lookup values s)
  | Var(id) ->
	  try
		SM.find id e
	  with
		Not_found -> raise (Undefined_variable(id))

let transform_program tapes values procedures p =
  let rec tfst e st = match st with
	`Statement(_,st) -> tfst  e st
  | `Sequence(stl) -> `Sequence(List.map (tfst e) stl)
  | `While(c,st) ->
      (* create a new anonymous procedure *)
	  let ag = List.map (fun i -> Argument i) (IS.elements (args_of_st e IS.empty st)) in
	  let iota_args m =
		let rec loop i r =
		  if i < 0 then
			r
		  else
			loop (i - 1) ((Argument i)::r)
		in
		loop (m - 1) []
	  in
	  let arity = List.length ag in
	  let ag' = iota_args arity in
	  let p =
		C.register_anonymous_fixpoint procedures
		  (fun p ->
			{ body =
			  `If(tfc e c,
				  `Sequence[tfst e st;
							`Apply(p,ag')],
				  `Sequence[]);
			  arity = arity })
	  in
	  `Apply(p,ag)
  | `Op(o) -> `Op(tfop e o)
  | `Let(l,st) ->
		(* bindings are non-mutually visible *)
	  let e = List.fold_left (fun e (id,xp) -> SM.add id (resolve_value values e xp) e) e l in
	  tfst e st
  | `Apply(id,l) ->
	  begin
		(* find last procedure with name id in procedures catalog *)
		try
		  let p = C.lookup procedures id in
		  let pi = C.find procedures p in
		  if pi.arity <> List.length l then
			raise (Mismatched_arity(id))
		  else
			`Apply(C.lookup procedures id,List.map (tfxp e) l)
		with
		  Not_found -> raise (Undefined_procedure(id))
	  end
  | `If(c,st1,st2) -> `If(tfc e c, tfst e st1, tfst e st2)
  and tfc e c = Boolean.map (tft e) c
  and tft e = function
	  (`Empty|`Eof|`Home) as x -> x
	| `Marked(tp,xp) -> `Marked(C.lookup tapes tp, tfxp e xp)
	| `See(xp) -> `See(tfxp e xp)
	| `Top(xp) -> `Top(tfxp e xp)
	| `Relation(r,xp1,xp2) -> `Relation(r,tfxp e xp1,tfxp e xp2)
  and tfop e = function
	  Push(xp) -> Push(tfxp e xp)
	| Mark(tp,xp) -> Mark(C.lookup tapes tp, tfxp e xp)
	| Output(xp) -> Output(tfxp e xp)
	| (Pop|Left|Right|Accept|Reject|Halt) as x -> x
  and tfxp e = function
	  Expression(_,xp) -> tfxp e xp
	| String(s) -> Constant(C.lookup values s)
	| Var(id) ->
		try
		  SM.find id e
		with
		  Not_found -> raise (Undefined_variable(id))
  and tftb e = function
	  Value(l) -> List.fold_left (fun e (id,xp) -> SM.add id (tfxp e xp) e) e l
	| Procedure(false,l) ->
		(* non mutually recursive procedure binding *)
		List.iter
		  (fun (id,params,st) ->
			let (arity,e) = List.fold_left (fun (i,e) id -> (i + 1,SM.add id (Argument i) e)) (0,e) params in
			let st = tfst e st in
			ignore (C.register_overriding procedures id
					  { body = st;
						arity = arity })) l;
		e
	| Procedure(true,l) ->
		(* mutually recursive procedure binding *)
		let a = Array.of_list l in
		let m = Array.length a in
		ignore
		  (C.register_fixpoints procedures
			 (Array.map
				(fun (id,params,_) ->
				  (id,
				   { body = `Sequence[];
					 arity = List.length params })) a)
			 (fun i p ->
			   let (id,params,st) = a.(i) in
			   let (arity,e) = List.fold_left (fun (i,e) id -> (i + 1,SM.add id (Argument i) e)) (0,e) params in
			   let st = tfst e st in
			   { body = st;
				 arity = arity }));
		e
	| Tape(t) -> e
  in
  ignore (List.fold_left tftb SM.empty p)


