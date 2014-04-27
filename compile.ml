(* Compile *)
(* $Id: compile.ml,v 1.21 2002/10/16 15:28:16 durak Exp $ *)

open Ast
open Intermediate

exception Compilation_failure

module C = Catalog.Make(String)

let unopt = function None -> "(none)" | Some(x) -> Printf.sprintf "%S" x

let compile (start,(p : (Ast.tape, Ast.expr, Ast.ident, (Ast.tape, Ast.expr) Ast.extended_test Boolean.t) Ast.toplevel_binding list)) =
  (* first, internalize all strings *)
  let values = C.create ()
  and tapes = C.create ()
  in
  let rec cvst = function
	  `Statement(_,st) -> cvst st
	| `Sequence(stl) -> List.iter cvst stl
	| `While(c,st) -> cvc c; cvst st
	| `Op(o) -> cvop o
	| `Let(l,st) ->
		List.iter (fun (_,xp) -> cvxp xp) l;
		cvst st
	| `Apply(_,l) -> List.iter cvxp l
	| `If(c,st1,st2) -> cvc c; cvst st1; cvst st2
  and cvc c = Boolean.iter cvt c
  and cvt = function
	  `Empty|`Eof|`Home -> ()
	| `Marked(_,xp)|`See(xp)|`Top(xp) -> cvxp xp
	| `Relation(_,xp1,xp2) -> cvxp xp1; cvxp xp2
  and cvop = function
	  Push(xp)|Mark(_,xp)|Output(xp) -> cvxp xp
	| Pop|Left|Right|Accept|Reject|Halt -> ()
  and cvxp = function
	  Expression(_,xp) -> cvxp xp
	| String(s) -> ignore (C.register values s ());
	| Var(_) -> ()
  and cvtb = function
	  Value(l) -> List.iter (fun (_,xp) -> cvxp xp) l
	| Procedure(_,l) -> List.iter (fun (_,_,st) -> cvst st) l
	| Tape(t) ->
		try
		  ignore (C.register_once tapes t ())
		with
		  C.Registered_twice -> raise (Tape_defined_twice t)
  in
  List.iter cvtb p;
  if !Opt.dump_values then
	begin
	  Printf.printf "Values:\n";
	  C.dump values stdout (fun x -> x) (fun _ _ -> ());
	end;
  if !Opt.dump_tapes then
	begin
	  Printf.printf "Tapes:\n";
	  C.dump tapes stdout (fun x -> x) (fun _ _ -> ());
	end;
  (* do it *)
  let procedures : procedure_info C.t = C.create () in
  Transform.transform_program tapes values procedures p;
  if !Opt.dump_intermediate then
	begin
	  Printf.printf "Procedures: (total %d)\n" (C.cardinality procedures);
	  C.dump procedures stdout
		(fun x -> x)
		(fun oc pi ->
		  Printf.fprintf oc "%d\n" pi.arity;
		  flush oc;
		  let f = Format.formatter_of_out_channel oc in
		  Format.fprintf f "          @[";
		  format_reduced_statement f pi.body;
		  Format.fprintf f "@]@.@?");
	end;
  begin
	try
	  Check.check_recursion procedures start;
	with
	  Check.Invalid_recursion(p1,p2) ->
		Printf.printf "Invalid recursion between procedures %s and %s.\n" p1 p2;
		raise Compilation_failure;
  end;
  let pg = Linearize.linearize procedures (C.lookup procedures start) in
  if !Opt.dump_linearized then
	begin
	  List.iter (function
		  `LABEL(l) -> Printf.printf "%s:\n" (Label.to_string l);
		| `BRA(c,l) -> Printf.printf "\tBRA(%s,%s)\n" (Opcodes.string_of_condition c) (Label.to_string l);
		| `OP(o) -> Printf.printf "\t%s\n" (Opcodes.string_of_op o)) pg;
	end;
  { Opcodes.number_of_tapes = C.cardinality tapes;
    Opcodes.code = Linearize.assemble pg },
  tapes,
  values,
  procedures

