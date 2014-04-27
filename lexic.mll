{
(* Lexic *)
(* $Id: lexic.mll,v 1.2 2002/10/15 13:39:50 durak Exp $ *)

open Syntax

let lexical_error l s = raise (Loc.Parse_error(Lexing.lexeme_start l, Lexing.lexeme_end l, s))

let hex_digit_to_int c =
  match c with
    '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
  | _ -> failwith ("Bad hexadecimal digit \""^(String.make 1 c)^"\".")

let decimal_digit_to_int c =
  match c with
    '0' .. '9' -> Char.code c - Char.code '0'
  | _ -> failwith ("Bad decimal digit \""^(String.make 1 c)^"\".")

let octal_digit_to_int c =
  match c with
    '0' .. '7' -> Char.code c - Char.code '0'
  | _ -> failwith ("Bad octal digit \""^(String.make 1 c)^"\".")

let binary_digit_to_int c =
  match c with
    '0' -> 0
  | '1' -> 1
  | _ -> failwith ("Bad binary digit \""^(String.make 1 c)^"\".")

let base_x_to_int s f x i =
  let m = String.length s in
  let rec loop i r =
    if i = m then r
    else
      loop (i + 1) ((r * x) + (f s.[i]))
  in
  loop i 0

let tfile l f =
  try
    f ()
  with
    Failure x -> lexical_error l x

let hex_to_int s i = base_x_to_int s hex_digit_to_int 16 i
let decimal_to_int s i = base_x_to_int s decimal_digit_to_int 10 i
let octal_to_int s i = base_x_to_int s octal_digit_to_int 8 i
let binary_to_int s i = base_x_to_int s binary_digit_to_int 2 i
}
let blancs = [' ' '\n' '\t' '\r']+
let alpha = ['a'-'z''A'-'Z']
let alphanum = alpha | ['0'-'9']
let decimaldigit = ['0'-'9']
let hexdigit = ['0'-'9''a'-'z''A'-'Z']
let octaldigit = ['0'-'7']
let binarydigit = ['0''1']
let digit = hexdigit|octaldigit|binarydigit|decimaldigit
let sign = '+'|'-'
let space = [' ''\t''\r''\n']
rule token = parse
(* Operators *)
| "<>" { NEQ }
(* Various non-alphabetic symbols *)
| ";" { SEMICOLON }
| "=" { EQ }
| "()" { UNIT }
| "(" { LPAREN }
| ")" { RPAREN }
| "&&" { LAND }
| "||" { OR }
| "or" { OR }
| "\"" { STRING(string lexbuf) }
(* Identifiers ans keywords *)
| "let" { LET }
| "value" { VALUE }
| "procedure" { PROCEDURE }
| "start" { START }
| "and" { AND }
| "in" { IN }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "while" { WHILE }
| "do" { DO }
| "done" { DONE }
| "tape" { TAPE }
| "begin" { BEGIN }
| "end" { END }
| "home" { AT_HOME }
| "eof" { AT_EOF }
| "xor" { XOR }
| "not" { NOT }
| "top" { TOP }
| "rec" { REC }
| "true" { TRUE }
| "false" { FALSE }
| "empty" { EMPTY }
| "see" { SEE }
| "left" { LEFT }
| "right" { RIGHT }
| "accept" { ACCEPT }
| "reject" { REJECT }
| "output" { OUTPUT }
| "mark" { MARK }
| "marked" { MARKED }
| "with" { WITH }
| "push" { PUSH }
| "pop" { POP }
| alpha (alphanum|'_')* { IDENT(Lexing.lexeme lexbuf) }
(* Comments, space, strings and end of file *)
| "(*" { eat_comment lexbuf; token lexbuf }
| space+ { token lexbuf }
| eof { EOF }
| _ { lexical_error lexbuf
	(Printf.sprintf "Unexpected %S" (Lexing.lexeme lexbuf)) }
and eat_comment = parse
    "(*" { eat_comment lexbuf; eat_comment lexbuf }
|   "*)" { }
|   [^'(' '*']+ { eat_comment lexbuf }
|   _ { eat_comment lexbuf }
and string = parse
"\"" { "" }
| "\\" { let c = char_escape lexbuf in (String.make 1 c)^(string lexbuf) }
| [^'"' '\\']+ { let s = Lexing.lexeme lexbuf in s^(string lexbuf) }
and char_escape = parse
  "n" { '\n' }
| "r" { '\r' }
| "b" { '\b' }
| ['0'-'9']['0'-'9']['0'-'9'] { Char.chr (int_of_string (Lexing.lexeme lexbuf)) }
| _ { failwith "Bad escape character." }
and post_char_escape = parse
  "'" { }
| _ { failwith (Printf.sprintf "Expecting \"'\" in character literal, not %S." (Lexing.lexeme lexbuf)) }
{
(* Epilogue. *)
}
