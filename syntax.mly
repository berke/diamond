%{
(* Syntax *)
(* $Id: syntax.mly,v 1.6 2002/10/16 14:41:44 durak Exp $ *)
(* Prolog *)

open Ast
open Boolean

let file_name = ref ""

let set_file_name x = file_name := x

let parse_error _ =
  let s = Parsing.symbol_start () and
      e = Parsing.symbol_end () in
  raise (Loc.Parse_error(s,e,"Syntax error"))
    
let current_location () = (Parsing.symbol_start (), Parsing.symbol_end ())

let make_expression x = `Expression(current_location (), x)
let make_statement x = `Statement(current_location (), x)

let sequencify = function
	[x] -> x
  | y -> `Sequence y
%}
%token SEMICOLON
%token EQ
%token NEQ
%token LEQ
%token GEQ
%token LT
%token GT
%token LPAREN
%token RPAREN
%token EOF
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO
%token DONE
%token LET
%token IN
%token UNIT
%token BEGIN
%token END
%token AT_EOF
%token AT_HOME
%token VALUE
%token PROCEDURE
%token START
%token TAPE
%token REC
%token AND
%token TRUE
%token FALSE
%token NOT
%token LAND
%token OR
%token XOR
%token EMPTY
%token SEE
%token TOP
%token LEFT
%token RIGHT
%token ACCEPT
%token REJECT
%token OUTPUT
%token MARK
%token MARKED
%token WITH
%token PUSH
%token POP
%token <string> IDENT
%token <string> STRING
%type <Ast.plain_program> program
%start program

/* Le séquençage a la plus faible priorité. */
%nonassoc P_sequence
%left SEMICOLON

%nonassoc OF

%left COMMA

/* Si on considère que ELSE est un opérateur associant à gauche, on */
/* résout l'ambiguïté des expressions IF THEN ELSE imbriquées */
/* de la bonne manière. */
/* %left IF */

/* Fait empirique : THEN doit venir avant ELSE, sinon erreur de syntaxe. */
%left THEN
%left ELSE

/* L'opérateur := associe à gauche */
/* L'EXPRESSION a := b := c := d */
/*  SIGNIFIE Affect(a,Affect(b,Affect(c,d))) */
/*  ET PAS Affect(Affect(Affect(a,b),c),d) */
%left COLONEQ

/* L'opérateur | associe à gauche (-19) */
/* L'EXPRESSION a|b|c */
/*  SIGNIFIE Ou(a,Ou(b,c)) */
/*  ET PAS Ou(Ou(a,b),c) */
%left OR

/* De même pour l'opérateur & */
/* L'EXPRESSION a|b|c */
/*  SIGNIFIE Ou(a,Ou(b,c)) */
/*  ET PAS Ou(Ou(a,b),c) */
%left LAND

%nonassoc NOT

/* Les opérateurs de comparaison sont, idéalement, non associatifs. */
/* Mais chez Cosmo seul '=' est non-associatif. */
/* D'après la BNF, tous les opérateurs sont associatifs. */
/* ILLEGAL a = b = c */
%nonassoc EQ
%nonassoc NEQ /* associatif chez Cosmo ... 29 conflits */
%left LT
%left LEQ
%left GT
%left GEQ

%left PLUS MINUS
%left STAR SLASH

%left DOT

%nonassoc P_unary_minus
%nonassoc P_id_value
%nonassoc P_id_bracketed_expression

%left LBRACK

%%
program :
  toplevel_bindings START IDENT EOF { $3,$1 }

toplevel_bindings :
  toplevel_binding toplevel_bindings { $1::$2 }
| toplevel_binding { [$1] }

toplevel_binding :
  VALUE value_binding_seq { Value($2) }
| PROCEDURE procedure_binding_seq { Procedure(false,$2) }
| PROCEDURE REC procedure_binding_seq { Procedure(true,$3) }
| TAPE IDENT { Tape($2) }

value_binding_seq :
  value_binding { [$1] }
| value_binding AND value_binding_seq { $1::$3 }

procedure_binding_seq :
  procedure_binding { [$1] }
| procedure_binding AND procedure_binding_seq { $1::$3 }

value_binding :
  IDENT EQ expr { ($1,$3) }

procedure_binding :
  IDENT EQ statement_list { ($1,[],sequencify $3) }
| IDENT parameters EQ statement_list { ($1,$2,sequencify $4) }

parameters : 
  parameter { [$1] }
| parameter parameters { $1::$2 }

parameter :
  IDENT { $1 }

parameters : 
  parameter { [$1] }
| parameter parameters { $1::$2 }

expressions :
  expr { [$1] }
| expr expressions { $1::$2 }

statement :
  WHILE condition DO statement_list DONE { `While($2,sequencify $4) }
| IF condition THEN statement ELSE statement { `If($2,$4,$6) }
| operation { `Op($1) }
| LET IDENT EQ expr IN statement_list { `Let([$2,$4],sequencify $6) }
| IDENT UNIT { `Apply($1,[]) }
| IDENT expressions { `Apply($1,$2) }
| BEGIN statement_list END { sequencify $2 }
| UNIT { `Sequence[] }

operation :
  PUSH expr { Push($2) }
| POP { Pop }
| LEFT { Left }
| RIGHT { Right }
| MARK IDENT WITH expr { Mark($2,$4) }
| ACCEPT { Accept }
| REJECT { Reject }
| OUTPUT expr { Output($2) }

statement_list :
  statement { [$1] }
| statement SEMICOLON statement_list { $1::$3 }

condition :
  LPAREN condition RPAREN { $2 }
| condition LAND condition { And($1,$3) }
| condition OR condition { Or($1,$3) }
| condition XOR condition { Xor($1,$3) }
| NOT condition { Not($2) }
| TRUE { True }
| FALSE { False }
| EMPTY { Atom(`Empty) }
| AT_EOF { Atom(`Eof) }
| AT_HOME { Atom(`Home) }
| SEE expr { Atom(`See($2)) }
| TOP expr { Atom(`Top($2)) }
| IDENT MARKED WITH expr { Atom(`Marked($1,$4)) }
| expr EQ expr { Atom(`Relation(Equal,$1,$3)) }
| expr NEQ expr { Not(Atom(`Relation(Equal,$1,$3))) }
| expr LEQ expr { Not(Atom(`Relation(Less,$1,$3))) }
| expr GEQ expr { Not(Atom(`Relation(Less,$3,$1))) }
expr :
  STRING { String($1) }
| IDENT { Var($1) }
| LPAREN expr RPAREN { $2 }
