(* Intermediate *)
(* $Id: intermediate.ml,v 1.8 2002/10/16 15:28:16 durak Exp $ *)

open Ast

type reduced_expr = Argument of int | Constant of int
and ('tape,'expr,'procedure_id) reduced_statement =
[   `Sequence of ('tape,'expr,'procedure_id) reduced_statement list
  | `Op of ('tape,'expr) operation
  | `Apply of 'procedure_id * 'expr list
  | `If of
	  ('tape,'expr) extended_test Boolean.t *
		('tape,'expr,'procedure_id) reduced_statement *
		('tape,'expr,'procedure_id) reduced_statement ]
and procedure_id = int

type procedure_info = {
	body : (int,reduced_expr,procedure_id) reduced_statement;
	arity : int
  }

exception Invalid_recursion of string * string list

type valeur = int

exception Undefined_variable of string
exception Undefined_procedure of string
exception Tape_defined_twice of string
exception Mismatched_arity of string

module SS = Set.Make(String)
module SM = Map.Make(String)
module IS = Set.Make(struct type t = int let compare = compare end)
module IM = Map.Make(struct type t = int let compare = compare end)
module PM = Map.Make(struct type t = procedure_id * expr list * Label.t option let compare = compare end)
module PS = Set.Make(struct type t = procedure_id * expr list let compare = compare end)
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

let format_list f g l =
  let first = ref false in
  Format.fprintf f "[@[";
  List.iter (fun x ->
	if !first then
	  Format.fprintf f ";@,";
	first := true;
	g f x) l;
  Format.fprintf f "@]]"

let rec format_reduced_statement f = function
	`Sequence(rl) ->
	  Format.fprintf f "`Sequence";
	  format_list f format_reduced_statement rl
  |	`Op(op) ->
	  Format.fprintf f "`Op(@[";
	  format_op f op;
	  Format.fprintf f "@])"
  |	`Apply(pid,xl) ->
	  Format.fprintf f "`Apply(%d," pid;
	  format_list f format_reduced_expr xl;
	  Format.fprintf f ")"
  |	`If(c,st1,st2) ->
	  Format.fprintf f "`If(";
	  format_condition f c;
	  Format.fprintf f ",@[";
	  format_reduced_statement f st1;
	  Format.fprintf f "@],@,@[";
	  Format.fprintf f "@[";
	  format_reduced_statement f st2;
	  Format.fprintf f "@])"
and format_op f = function
	Push(xp) -> Format.fprintf f "Push(@["; format_reduced_expr f xp; Format.fprintf f "@])"
  | Mark(tp,xp) -> Format.fprintf f "Mark(%d,@[" tp; format_reduced_expr f xp; Format.fprintf f "@])"
  | Pop -> Format.fprintf f "Pop"
  | Left -> Format.fprintf f "Left"
  | Right -> Format.fprintf f "Right"
  | Accept -> Format.fprintf f "Accept"
  | Halt -> Format.fprintf f "Halt"
  | Reject -> Format.fprintf f "Reject"
  | Output(xp) -> Format.fprintf f "Output(@["; format_reduced_expr f xp; Format.fprintf f "@])"
and format_reduced_expr f = function
	Constant(i) -> Format.fprintf f "Constant(%d)" i
  | Argument(i) -> Format.fprintf f "Argument(%d)" i
and format_test f = function
	`Empty -> Format.fprintf f "Empty"
  | `Eof -> Format.fprintf f "Eof"
  | `Home -> Format.fprintf f "Home"
  | `Marked(tp,xp) -> Format.fprintf f "Marked(%d,@[" tp; format_reduced_expr f xp; Format.fprintf f "@])"
  | `Relation(rl,xp1,xp2) ->
	  Format.fprintf f "Relation(%s,@["
		(match rl with
		  Equal -> "Equal"
		| Less -> "Less");
	  format_reduced_expr f xp1;
	  Format.fprintf f "@],@[";
	  format_reduced_expr f xp2;
	  Format.fprintf f "@])"
  | `See(xp) -> Format.fprintf f "See(@["; format_reduced_expr f xp; Format.fprintf f "@])"
  | `Top(xp) -> Format.fprintf f "Top(@["; format_reduced_expr f xp; Format.fprintf f "@])"
and format_condition f = function
	Boolean.Atom(t) -> format_test f t
  |	Boolean.And(c1,c2) ->
	  Format.fprintf f "And(@[";
	  format_condition f c1;
	  Format.fprintf f "@],@[";
	  format_condition f c2;
	  Format.fprintf f "@])"
  |	Boolean.Or(c1,c2) ->
	  Format.fprintf f "Or(@[";
	  format_condition f c1;
	  Format.fprintf f "@],@[";
	  format_condition f c2;
	  Format.fprintf f "@])"
  |	Boolean.Xor(c1,c2) ->
	  Format.fprintf f "Xor(@[";
	  format_condition f c1;
	  Format.fprintf f "@],@[";
	  format_condition f c2;
	  Format.fprintf f "@])"
  |	Boolean.Not(c) -> Format.fprintf f "Not(@["; format_condition f c; Format.fprintf f "@])"
  |	Boolean.True -> Format.fprintf f "True"
  |	Boolean.False -> Format.fprintf f "False"
