(* Intermediate *)
(* $Id: intermediate.mli,v 1.6 2002/10/16 13:52:20 durak Exp $ *)

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
and procedure_info = {
  body : (int, reduced_expr, procedure_id) reduced_statement;
  arity : int;
} 
exception Invalid_recursion of string * string list

type valeur = int

exception Undefined_variable of string
exception Undefined_procedure of string
exception Tape_defined_twice of string
exception Mismatched_arity of string

val format_reduced_statement : Format.formatter -> (int,reduced_expr,procedure_id) reduced_statement -> unit
