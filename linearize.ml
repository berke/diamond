(* Linearize *)
(* $Id: linearize.ml,v 1.9 2002/10/16 13:52:21 durak Exp $ *)

open Ast
open Intermediate

module PS = Set.Make(struct type t = procedure_id * valeur list let compare = compare end)
module PM = Map.Make(struct type t = procedure_id * valeur list let compare = compare end)
module LM =
  struct
	include Map.Make(Label)
	exception Label_not_found of string
	let find x lm =
	  try
		find x lm
	  with
		Not_found -> raise (Label_not_found(Label.to_string x))
  end
module C = Catalog.Make(String)

let lopt_to_string = function
	None -> "(none)"
  |	Some(x) -> Label.to_string x

module FC = Catalog.Make(struct type t = (int,reduced_expr) operation list let compare = compare end)

(* return the index of y in list l *)
(* raise Not_found if not found *)

let list_index l y =
  let rec loop i = function
	  [] -> raise Not_found
	| x::rest ->
		if x = y then
		  i
		else
		  loop (i + 1) rest
  in
  loop 0

let rec is_empty = function
	[] -> true
  |	x::y ->
	  (match x with
		`Sequence(r) -> is_empty r
	  |	_ -> false) && (is_empty y)

let instantiate_reduced_expression ~(args : valeur list) = function
	Argument(i) -> List.nth args i
  |	Constant(x) -> x

let instantiate_operation args = function
	Mark(x,y) -> Mark(x,instantiate_reduced_expression args y)
  | (Left|Right|Accept|Reject|Pop|Halt) as x -> x
  |	Push(x) -> Push(instantiate_reduced_expression args x)
  | Output(x) -> Output(instantiate_reduced_expression args x)

type irtest = (int,reduced_expr) test

let instantiate_test ~(args : valeur list) = function
	(`Empty|`Eof|`Home) as x -> x
  |	`Marked(t,xp) -> `Marked(t,instantiate_reduced_expression ~args xp)
  |	`See(xp) -> `See(instantiate_reduced_expression ~args xp)
  |	`Top(xp) -> `Top(instantiate_reduced_expression ~args xp)

let instantiate_condition ~(args : valeur list) ~cond : (int,Intermediate.valeur) Ast.test Boolean.t =
  Boolean.simplify
	(function
		`Relation(rel,x1,x2) ->
		  let e1 = instantiate_reduced_expression args x1
		  and e2 = instantiate_reduced_expression args x2
		  in
		  begin
			if
			  match rel with
				Equal -> e1 = e2
			  | Less -> e1 <= e2
			then
			  Boolean.Always
			else
			  Boolean.Never
		  end
	  | # irtest as x -> Boolean.Maybe(instantiate_test ~args x)) cond

let sf = Printf.sprintf
		
type ('label,'target) intermediate = [
	`BRA of Opcodes.condition * 'target
  |	`OP of Opcodes.operation
  |	`LABEL of 'label
]

let linearize procedures start =
  let rec genst pm args = function
	  [] -> []
	| `Sequence(l)::rest -> genst pm args (l@rest)
	| `If(cond,st1,st2)::rest ->
		let l1 = Label.next ()
		and l2 = Label.next ()
		in
		[`BRA(instantiate_condition ~args ~cond,l1)]@
		(genst pm args [st2])@
		[`BRA(Boolean.True,l2)]@
		[`LABEL(l1)]@
		(genst pm args [st1])@
		[`LABEL(l2)]@
		(genst pm args rest)
	| `Op(o)::rest -> (`OP(instantiate_operation args o))::(genst pm args rest)
	| `Apply(pid,args')::rest ->
		let args'' = List.map (instantiate_reduced_expression ~args) args' in
		if PM.mem (pid,args'') pm then
		  (* must be tail call, throw rest *)
		  [`BRA(Boolean.True,PM.find (pid,args'') pm)]
		else
		  let l = Label.next () in
		  [`LABEL(l)]@
		  (genst
			 (PM.add (pid,args'') l pm)
			 args''
			 [(C.find procedures pid).body])@
		  (genst pm args rest)
  in
  genst PM.empty [] [`Apply(start,[])]

let assemble (pg : (Label.t,Label.t) intermediate list)  =
  let rec collect i lb = function
	  `LABEL(l)::rest -> collect i (LM.add l i lb) rest
	| _::rest -> collect (i + 1) lb rest
	| [] -> (i,lb)
  in
  let (m,lb) = collect 0 LM.empty pg in
  let a = Array.make m (`OP Halt) in
  let rec emit i = function
	  `LABEL(_)::rest -> emit i rest
	| `OP(o)::rest -> a.(i) <- `OP(o); emit (i + 1) rest
	| `BRA(c,l)::rest -> a.(i) <- `BRA(c,LM.find l lb); emit (i + 1) rest
	| [] -> ()
  in
  emit 0 pg;
  a

  
  
