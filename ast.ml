(* AST *)
(* $Id: ast.ml,v 1.4 2002/10/16 13:52:20 durak Exp $ *)

type location = Loc.t
and ident = string
and expr = Expression of location * expr | String of string | Var of ident
and tape = string

type ('tape,'expr,'ident,'condition) program = 'ident * ('tape,'expr,'ident,'condition) toplevel_binding list
and ('tape,'expr,'ident,'condition) toplevel_binding =
	Value of ('ident * 'expr) list
  | Tape of 'tape
  | Procedure of bool * ('ident * 'ident list * ('tape,'expr,'ident,'condition) statement) list
and ('tape,'expr,'ident,'condition) statement =
[  `Statement of location * ('tape,'expr,'ident,'condition) statement
  | `Sequence of ('tape,'expr,'ident,'condition) statement list
  | `While of 'condition * ('tape,'expr,'ident,'condition) statement
  | `Op of ('tape,'expr) operation
  | `Let of ('ident * 'expr) list * ('tape,'expr,'ident,'condition) statement
  | `Apply of 'ident * 'expr list
  | `If of 'condition * ('tape,'expr,'ident,'condition) statement * ('tape,'expr,'ident,'condition) statement ]
and ('tape,'expr) condition = ('tape,'expr) test Boolean.t
and ('tape,'expr) test = [
	`Empty
  | `Eof
  | `Home
  | `Marked of 'tape * 'expr
  | `See of 'expr
  | `Top of 'expr
]
and ('tape,'expr) operation =
  Push of 'expr
| Mark of 'tape * 'expr
| Pop
| Left
| Right
| Accept
| Reject
| Output of 'expr
| Halt
type ('tape,'expr) extended_test = [
	('tape,'expr) test
  | `Relation of relation * 'expr * 'expr
]
and relation = Equal | Less

type plain_program = (tape,expr,ident,(tape,expr) extended_test Boolean.t) program
