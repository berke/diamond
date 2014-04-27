(* Micro *)
(* $Id: micro.ml,v 1.12 2002/10/11 12:05:22 durak Exp $ *)

open Ast
open Opcodes

module Label :
	sig
	  type t
	  val next : unit -> t
	  val compare : t -> t -> int
	  val to_string : t -> string
	end
	=
  struct
	type t = int
	let counter = ref 0
	let next () =
	  incr counter;
	  !counter - 1
	let compare = compare
	let to_string l = Printf.sprintf "l%d" l
  end

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

module Labels =
  struct
	module LM = Map.Make(struct type t = int let compare = compare end)
	type 'a t = {
		counter : int;
		map : 'a LM.t
	  }	
	exception Label_not_found of string
	let empty = { counter = 0; map = LM.empty }
	let next lb = ({ lb with counter = lb.counter + 1 },lb.counter)
	let add lb x y = { lb with map = LM.add x y lb.map }
	let iter lb f = LM.iter f lb.map
	let to_string x = Printf.sprintf "l%d" x
	let find lb x =
	  try
		LM.find x lb.map
	  with
		Not_found -> raise (Label_not_found(to_string x))
  end

type st = [
	`Sequence of st list
  |	`If of condition * st * st
  |	`Op of (int,int) Ast.operation
  |	`Call of pid * expr list
  ]
and si = [
	Label.t instruction
  |	`LABEL of Label.t ]
and pid = string
and expr = string

module SM = Map.Make(String)

let linearize pg =
  let rec f = function
	  [] -> []
	| x::rest ->
		match x with
		  `Sequence(l) -> f (l@rest)
		| `If(cond,st1,st2) ->
			let lb1 = Label.next ()
			and lb2 = Label.next ()
			in
			List.concat
			  [
			   [`BRA(cond,lb1)];
			   (f [st1]);
			   [`BRA(Boolean.True,lb2)];
			   [`LABEL lb1];
			   (f [st2]);
			   [`LABEL lb2];
			   (f rest)]
		| `Op(op) ->
			(`OP(match op with
			  Push(x) -> PUSH(x)
			| Mark(t,x) -> MARK(t,x)
			| Pop -> POP
			| Left -> LEFT
			| Right -> RIGHT
			| Accept -> ACCEPT
			| Reject -> REJECT
			| Output(x) -> OUTPUT(x)))::(f rest)
  in
  let il = f [pg] in
  let (m,lb) =
	List.fold_left (fun (i,lb) -> function
		`LABEL(l) -> (i,LM.add l i lb)
	  | `OP(_) -> (i + 1,lb)
	  | `BRA(_,_) -> (i + 1,lb)) (0,LM.empty) il
  in
  let a = Array.make m (`OP ACCEPT) in
  ignore (List.fold_left (fun i -> function
	  `LABEL(_) -> i
	| `OP(o) -> a.(i) <- `OP(o); i + 1
	| `BRA(c,l) -> a.(i) <- `BRA(c,LM.find l lb); i + 1) 0 il);
  a

let translate_op = function
	Push(x) -> PUSH(x)
  | Mark(t,x) -> MARK(t,x)
  | Pop -> POP
  | Left -> LEFT
  | Right -> RIGHT
  | Accept -> ACCEPT
  | Reject -> REJECT
  | Output(x) -> OUTPUT(x)

type continuation = [
	`Conditional of condition * Label.t option * Label.t option
  |	`Unconditional of Label.t option
  |	`Halt ]

module PM = Map.Make(struct type t = pid * expr list * Label.t option let compare = compare end)
module PS = Set.Make(struct type t = pid * expr list let compare = compare end)

let lopt_to_string = function
	None -> "(none)"
  |	Some(x) -> Label.to_string x

let continuation_to_string = function
	`Conditional(c,l1,l2) -> Printf.sprintf "C(_,%s,%s)" (lopt_to_string l1) (lopt_to_string l2)
  |	`Unconditional(l) -> Printf.sprintf "U(%s)" (lopt_to_string l)
  |	`Halt -> "H"

(* let linearize2 pg =
  let lb = ref LM.empty in
  let rec f continuation = function
	  [] -> continuation
	| x::rest ->
		match x with
		  `Sequence(l) -> f continuation (l@rest)
		| `If(cond,st1,st2) ->
			let l1 = f continuation [st1]
			and l2 = f continuation [st2]
			and l3 = Label.next ()
			in
			lb := LM.add l3 ([],`Conditional(cond,l1,l2)) !lb;
			l3
		| `Op(o) -> do_ops continuation [translate_op o] rest
  and do_ops continuation result = function
	  (`If(_,_,_)::_|[]) as things ->
	   let l = Label.next () in
	   let y = f continuation things in
	   lb := LM.add l (List.rev result,`Unconditional(y)) !lb;
	   l
	| `Sequence(s)::rest -> do_ops continuation result (s@rest)
	| `Op(o)::rest -> do_ops continuation ((translate_op o)::result) rest
  in
  let l0 = Label.next () in
  let start = f l0 [pg] in
  LM.iter (fun l (x,cont) ->
	Printf.printf "Label %S\n" (Label.to_string l);
	Printf.printf "\tContinuation %S\n" (continuation_to_string cont);
	Printf.printf "\tTotal %d instructions\n" (List.length x)) !lb *)

(********************)

let rec is_empty = function
	[] -> true
  |	x::y ->
	  (match x with
		`Sequence(r) -> is_empty r
	  |	_ -> false) && (is_empty y)

exception Invalid_recursion of string * string list

type fragment = {
	code : deferred Opcodes.instruction list; (* !!! Label.t instruction list ? *)
	continuation : continuation;
	mutable location : int; (* where will fragment will be placed ? *)
	mutable mark : bool
  }
and deferred = [
	`Procedure of pid * expr list * Label.t option
  |	`Label of Label.t option
] 

let linearize4 (procedures : st SM.t) start =
  let lb = ref LM.empty in
  let instantiated = ref PM.empty in
  let rec instantiate_statement pending continuation = function
	  [] -> continuation
	| x::rest ->
		match x with
		  `Sequence(l) -> instantiate_statement pending continuation (l@rest)
		| `If(cond,st1,st2) ->
			let l1 = instantiate_statement pending continuation [st1]
			and l2 = instantiate_statement pending continuation [st2]
			and l3 = Label.next ()
			in
			lb :=
			  LM.add l3 
				{ code = [];
				  continuation = `Conditional(cond,l1,l2);
				  location = -1;
				  mark = false } !lb;
			Some l3
		| `Op(o) -> do_ops pending continuation [`OP(translate_op o)] rest
		| `Call(pid,args) ->
			if is_empty rest then
			  (* tail call *)
			  if PS.mem (pid,args) pending then
				begin
				  Printf.printf "tail-recursive call to %s\n" pid;
				  do_ops pending continuation [`BRA(Boolean.True,`Procedure (pid,args,continuation))] rest
				end
			  else
				begin
				  Printf.printf "inserting call to %s\n" pid;
				  instantiate_procedure pending pid args continuation
				end
			else
			  (* sequenced call, must not be pending *)
			  begin
				Printf.printf "sequenced call to %s\n" pid;
				if PS.mem (pid,args) pending then
				  raise (Invalid_recursion("sequenced",
										   (List.map
											  (fun (pid,args) ->
												Printf.sprintf "%s(%s)"
												  pid
												  (String.concat "," args))
											  (PS.elements pending))))
					
				else
				  let l1 = instantiate_statement pending continuation rest in
				  instantiate_procedure pending pid args (* (Some l1) *) l1
			  end
  and instantiate_procedure pending pid args continuation =
	Printf.printf "instantiate %s [%s] %s\n" pid
	  (String.concat ";" args)
	  (match continuation with None -> "(none)" | Some(x) -> Label.to_string x);
	flush stdout;
	if PS.mem (pid,args) pending then
	  raise (Invalid_recursion("instantiate",
							   (List.map
								  (fun (pid,args) ->
									Printf.sprintf "%s(%s)"
									  pid
									  (String.concat "," args))
								  (PS.elements pending))))
	else
	  try
		PM.find (pid,args,continuation) !instantiated
	  with
		Not_found ->
		  let (st : st) = SM.find pid procedures in
		  let l = instantiate_statement (PS.add (pid,args) pending) continuation [st] in
		  instantiated := PM.add (pid,args,continuation) l !instantiated;
		  l
  and do_ops pending (continuation : Label.t option) result = function
	  ((`If(_,_,_)|`Call(_,_))::_|[]) as things ->
	   let l = Label.next () in
	   let y = instantiate_statement pending continuation things in
	   lb := LM.add l
		   { code = List.rev result;
			 continuation = `Unconditional(y);
			 location = -1;
			 mark = false } !lb;
	   Some l
	| `Sequence(s)::rest -> do_ops pending continuation result (s@rest)
	| `Op(o)::rest -> do_ops pending continuation ((`OP(translate_op o))::result) rest
  in
  (* let l0 = Label.next () in *)
  let start = instantiate_procedure PS.empty start [] None in
  let is_placed fg = fg.location >= 0 in
  (* arguments : *)
  (*   result : instructions in slots 0...i-1 *)
  (*   i : first available instruction slot *)
  (*   fg : fragment to place *)
  (* post-conditions : *)
  (*   1) fragment fg must be placed at position i.. if not already placed (check fg.location) *)
  (*   2) its continuations must also be placed at appropriate places *)
  (*   3) eventual branches to its continuations must be emitted *)
  (*   4) the result is int * instruction list : *)
  (*      the first available instruction slot, *)
  (*      the instructions in slots 0...i-1 *)
  let rec place i result fg =
	if fg.location >= 0 then
	  (* this fragment has already been placed *)
	  (i,result)
	else
	  (* this fragment has not yet been placed. *)
	  (* place it right here *)
	  begin
		fg.location <- i;
		let result = (fg.code)::result
		and m = List.length fg.code
		in
		match fg.continuation with
		  (`Unconditional(None)|`Halt) ->
			(* this fragment has no continuation *)
			(* CHECK : EMIT HALT INSTRUCTION ? *)
			(i + m,result)
		| `Unconditional(Some(l)) ->
			let fg' = LM.find l !lb in
			if fg'.location >= 0 then
			  (i + m + 1,[`BRA(Boolean.True,`Label(Some l))]::result)
			else
			  place (i + m) result fg'
		| `Conditional(_,None,_)|`Conditional(_,_,None) -> assert false
		| `Conditional(c,Some(l1),Some(l2)) ->
			let fg1 = LM.find l1 !lb
			and fg2 = LM.find l2 !lb
			in
			(* if either fg1 or fg2 is placed immediately after... *)
			match (fg1.location >= 0,fg2.location >= 0) with
			  (true,true) -> (i + m + 2,([`BRA(c,`Label(Some l1));`BRA(Boolean.True,`Label(Some l2))]::result))
			| (true,false) -> place (i + m + 1) ([`BRA(c,`Label(Some l1))]::result) fg2
			| (false,true) -> place (i + m + 1) ([`BRA(Boolean.Not(c),`Label(Some l2))]::result) fg1
			| (false,false) ->
				let (i',result') = place (i + m + 1) ([`BRA(c,`Label(Some l1))]::result) fg2 in
				place i' result' fg1
	  end
  in
  match start with
	None -> invalid_arg "none"
  |	Some(start) -> 
	  let fg_start = LM.find start !lb in
	  let (m,result) = place 0 [] fg_start in
	  LM.iter (fun l fg ->
		Printf.printf "Label %S\n" (Label.to_string l);
		Printf.printf "\tLocation %d\n" fg.location;
		Printf.printf "\tContinuation %s\n" (continuation_to_string fg.continuation);
		Printf.printf "\tHas %d instructions\n" (List.length fg.code);
		let rec loop i = function
			[] -> ()
		  |	ins::rest ->
			  let ins' = match ins with
				`BRA(cond,x) ->
				  begin
					match x with
					  `Procedure(p,a,c) -> `BRA(cond,PM.find (p,a,c) !instantiated)
					| `Label(l) -> `BRA(cond,l)
				  end
			  |	`OP(o) -> `OP(o)
			  in
			  Printf.printf "\t\t%-5d\t%s\n" i (Opcodes.string_of_instruction lopt_to_string ins');
			  loop (i + 1) rest
		in
		loop 0 fg.code) !lb;
	  Printf.printf "Linearized code has %d instructions.\n" m;
	  let code = Array.of_list (List.concat (List.rev result)) in
	  let m' = Array.length code in
	  assert (m = m');
	  for i = 0 to m - 1 do
		Printf.printf "\t%-05d\t%s\n" i
		  (Opcodes.string_of_instruction
			 (function
				 `Procedure(p,a,c) ->
				   begin
					 match PM.find (p,a,c) !instantiated with
					   None -> assert false
					 | Some(l) ->
						 let fg = LM.find l !lb in
						 sf "%d" fg.location
				   end
			   | `Label(None) -> "none"
			   | `Label(Some l) ->
				   let fg = LM.find l !lb in
				   sf "%d" fg.location)
			 code.(i))
	  done 
		
let rec iotaf n x f =
  if n = 0 then
	[]
  else
	(f x)::(iotaf (n - 1) x f)

let generate m n =
  let names = Array.init m (fun i -> Printf.sprintf "proc_%d" i) in
  let rec gen_code i n =
	if n = 0 then
	  match Random.int 2 with
		0 -> `Op(Push(Random.int 1000))
	  | _ -> `Call(names.(Random.int m),[])
	else
	  match Random.int 20 with
		0 -> `Op(Push(Random.int 1000))
	  | 1 -> `If(Boolean.True,
				 gen_code i (n - 1),
				 gen_code i (n - 1))
	  | 2 -> `Call(names.(Random.int m),iotaf (Random.int n) () (fun () -> Printf.sprintf "a%d" (Random.int 10)))
	  |	_ -> `Sequence(iotaf (Random.int n) (n - 1) (gen_code i))
  in
  let procedures = Array.init m (fun i -> gen_code i n) in
  Array.to_list (Array.mapi (fun i x -> names.(i),x) procedures)

let pg1 =
  `Sequence[`Sequence[`Op Pop;`Op Left;`Op(Push 33)];
			`If(Boolean.True,
				`Sequence[`Op(Push 1);`Sequence[`Op(Push 2);`Op(Push 3)];`Op(Push 4)],
				`Sequence[`Op(Push 5);`Sequence[`Op(Push 6);`Op(Push 7)];`Op(Push 8)]);
			`Sequence[`Op(Push 5);`Sequence[`Op(Push 6);`Op(Push 7)];`Op(Push 8)]]
  
let pg2 =
  ["gogol",
   `Sequence[`Sequence[`Op Pop;`Call("hello",["this";"was";"a";"test"]);`Op Left;`Op(Push 33)];
			 `If(Boolean.True,
				 `Sequence[`Op(Push 1);`Call("hello",["this";"is";"a";"test"]);
						   `Sequence[`Op(Push 2);`Op(Push 3)];`Op(Push 4)],
				 `Sequence[`Op(Push 5);`Sequence[`Op(Push 6);`Op(Push 7)];`Op(Push 8)]);
			 `Sequence[`Op(Push 5);`Sequence[`Op(Push 6);`Call("hello",["this";"is";"a";"test"]);
											 `Op(Push 7)];`Op(Push 8)]];
   "hello",
   `Sequence[`Call("toto",["was";"here"]);
			 `Call("hello",["x";"y";"z"])];

   "loop",
   `Sequence[`Op(Push 16);
			 `Call("loop",[])];

   "zozo",
   `Sequence[`Op Pop;`Op Left];

   "toto",
   `Sequence[`Sequence[`Op Pop;`Op Left;`Op(Push 33)];
			 `If(Boolean.True,
				 `Sequence[`Op(Push 1);
						   `Sequence[`Op(Push 2);`Call("zozo",["x"]);`Op(Push 3)];`Op(Push 4);`Call("toto",[]);`Sequence[]],
				 `Sequence[`Call("zozo",["x"]);`Op(Push 5);`Sequence[`Op(Push 6);`Op(Push 7)];`Op(Push 8)]);
			 `Sequence[`Op(Push 5);`Sequence[`Op(Push 6);`Call("zozo",["x"]);`Call("zozo",["this";"is";"a";"test"]);
											 `Op(Push 7)];`Op(Push 8)]]]

let pg3 = ["toto", `Sequence[`Call("toto",[]);`Call("toto",[])]]

let pg4 = [
  "node1",
  `Sequence[`Op(Push 123);
			`If(Boolean.Atom(SEE 'a'),
				`Call("node3",[]),
				`Call("node2",[]))];

  "node2",
  `Sequence[`Op(Push 132);
			`Call("node4",[])];

  "node3",
  `Sequence[`Op(Push 213);
			`Call("node4",[])];

  "node4",
  `Sequence[`Op(Push 231);
			`If(Boolean.Atom(SEE 'b'),
				`Call("node3",[]),
 				`Call("node1",[]))]
] 

let test pg start =
  let procedures = List.fold_left (fun sm (x,y) -> SM.add x y sm) SM.empty pg in
  linearize4 procedures start

let _ = test pg2 "toto"
