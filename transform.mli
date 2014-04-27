(* Transform *)
(* $Id: transform.mli,v 1.2 2002/10/11 15:54:09 durak Exp $ *)

open Ast
open Intermediate

val transform_program :
	'a Catalog.Make(String).t -> 'b Catalog.Make(String).t -> procedure_info Catalog.Make(String).t ->
	  (tape,expr,ident,(tape,expr) extended_test Boolean.t) Ast.toplevel_binding list -> unit
