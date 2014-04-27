(* Opcodes *)
(* $Id: opcodes.ml,v 1.12 2003/07/25 16:38:54 durak Exp $ *)
(* vim:set ts=4:*)

open Ast

type program = {
	number_of_tapes : int;
	code : int instruction array
  }
and 'target instruction = [
	`BRA of condition * 'target
  |	`OP of operation ]
and operation = (tape_id,symbol) Ast.operation
and condition = test Boolean.t
and test = (tape_id,symbol) Ast.test
and tape_id = int
and target = int
and symbol = int

(* stupid execution *)

type result = Automaton_accepts | Automaton_rejects | Automaton_is_halted of int * reason
and reason =
  Attempt_to_mark_out_of_bounds
| Attempt_to_mark_already_marked_case
| Attempt_to_move_head_beyond_limits
| Attempt_to_pop_empty_stack
| Halt_instruction

let pf = Printf.fprintf
let sf = Printf.sprintf

let string_of_test = function
	`See(c) -> sf "SEE(%d)" c
  | `Top(x) -> sf "TOP(%d)" x
  | `Marked(x,y) -> sf "MARKED(%d,%d)" x y
  | `Empty -> "EMPTY"
  | `Eof -> "EOF"
  | `Home -> "HOME"
	
let string_of_condition c = Boolean.to_string string_of_test c

let string_of_op = function
	Mark(x,y) -> sf "MARK(%d,%d)" x y
  | Left -> "LEFT"
  | Right -> "RIGHT"
  | Accept -> "ACCEPT"
  | Reject -> "REJECT"
  | Halt -> "HALT"
  | Pop -> "POP"
  | Push(x) -> sf "PUSH(%d)" x
  | Output(x) -> sf "OUTPUT(%d)" x

let string_of_instruction f = function
	`BRA(c,t) -> sf "BRA(%s,%s)" (string_of_condition c) (f t)
  |	`OP(o) -> string_of_op o

let dump oc p =
  pf oc "Tapes: %d\n" p.number_of_tapes;
  pf oc "Instructions: %d\n" (Array.length p.code);
  pf oc "Code:\n";
  for i = 0 to Array.length p.code - 1 do
	pf oc "\t%-05d\t%s\n" i (string_of_instruction (sf "%d") p.code.(i))
  done;
  pf oc "End of code.\n"

(* f : output_symbol -> unit *)

let execute f p w tb tracer =
  let m = String.length w in
  let n = Array.length p.code in
  let t = Array.make_matrix p.number_of_tapes m (-1) in (* -1 means unmarked *)
  (* j : head position, 0 = at_home, 1 .. m, m + 1 = eof; i : instruction pointer ; s : stack *)
  let u = p.code in
  let test j s = function
	  `See(c) -> j < m && tb.(Char.code w.[j]) = c
	| `Top(x) ->
		begin
		  match s with
			y::_ -> y = x
		  | _ -> false
		end
	| `Marked(h,x) ->
            if j < m then
              if t.(j).(j) < 0 then
                begin
                  t.(h).(j) <- 0;
                  x = 0 (* ``reading from a undefined position immediately makes
                             the position defined to contain an (arbitrary)
                             default symbol'' *)
                end
              else
                t.(h).(j) = x
            else
              false
	| `Empty -> s = []
	| `Home -> j = 0
	| `Eof -> j = m
  in
  let condition j s c = Boolean.eval (test j s) c in
  let rec loop i j s =
	tracer i j s;
	if i = n then
	  Automaton_rejects
	else
	  match u.(i) with
		`BRA(c,t) ->
		  if condition j s c then
			loop t j s
		  else
			loop (i + 1) j s
	  | `OP(o) ->
		  match o with
			Mark(h,x) ->
			  if m = 0 then
				Automaton_is_halted(i,Attempt_to_mark_out_of_bounds)
			  else
				if t.(h).(j) >= 0 then
				  Automaton_is_halted(i,Attempt_to_mark_already_marked_case)
				else
				  begin
					t.(h).(j) <- x;
					loop (i + 1) j s
				  end
		  | Left ->
			  if j > 0 then
				loop (i + 1) (j - 1) s
			  else
				Automaton_is_halted(i,Attempt_to_move_head_beyond_limits)
		  | Right ->
			  if j < m then
				loop (i + 1) (j + 1) s
			  else
				Automaton_is_halted(i,Attempt_to_move_head_beyond_limits)
		  | Accept -> Automaton_accepts
		  | Reject -> Automaton_rejects
		  | Pop ->
			  begin
				match s with
				  [] -> Automaton_is_halted(i,Attempt_to_pop_empty_stack)
				| _::s'-> loop (i + 1) j s'
			  end
		  | Push x ->
			  loop (i + 1) j (x::s)
		  | Output x ->
			  f x j;
			  loop (i + 1) j s
		  |	Halt -> Automaton_is_halted(i,Halt_instruction)
  in
  loop 0 0 []
