(* Main *)
(* $Id: main.ml,v 1.11 2003/07/25 16:38:54 durak Exp $ *)
(* vim:set ts=4: *)

let parse_file fn =
  let ic = open_in fn in
  try
	let l = Lexing.from_channel ic in
	try
	  Syntax.program Lexic.token l
	with
	  Loc.Parse_error(i,j,s) as x ->
		Printf.eprintf "File %S bytes %d-%d:\n%s\n" fn i j s;
		raise x
  with
	x ->
	  close_in ic;
	  raise x

module C = Catalog.Make(String)
		
let unoptstr = function None -> "(none)" | Some x -> x

let substitute env w =
  try
    ignore (String.index w '$');
    let m = String.length w in
    let b = Buffer.create m in
    let f g i =
      if i = m then
        Buffer.contents b
      else
        g i
    in
    let rec loop1 i = match w.[i] with
    | '$' -> f (loop2 (i + 1)) (i + 1)
    | c -> Buffer.add_char b c; f loop1 (i + 1)
    and loop2 i1 i = match w.[i] with
    | '{' -> f (loop3 (i1 + 1)) (i + 1)
    | '$' ->
        Buffer.add_char b '$';
        f loop1 (i + 1)
    | c -> loop4 i1 i (i + 1)
    and loop3 i1 i = match w.[i] with
    | '}' -> loop4 i1 (i - 1) (i + 1)
    | _ -> f (loop3 i1) (i + 1)
    and loop4 i1 i2 i =
      let x = String.sub w i1 (i2 - i1 + 1) in
      Buffer.add_string b
        (try
          List.assoc x env
         with
         | Not_found -> "?"^x^"?");
      f loop1 i
    in
    f loop1 0
  with
  | Not_found -> w

let process x =
  let ast = parse_file x in
  let pg,tapes,values,procedures = Compile.compile ast in
  let null_symbol = C.register values "" () in
  if !Opt.dump_code then
	begin
	  Opcodes.dump stdout pg
	end;
	  let process w =
		if !Opt.trace then Printf.printf "Running automaton on input string %S.\n" w;
		let table = Array.init 256 (fun x ->
		  let s = Printf.sprintf "%c" (Char.chr x) in
		  try
			C.lookup values s
		  with
			Not_found -> null_symbol)
		in
		let m = String.length w in
		let n = Array.length pg.Opcodes.code in
		  Opcodes.execute
		  (if !Opt.no_output then
			(fun _ _ -> ())
		  else
			(fun sym i ->
              let w = unoptstr (C.name_of values sym) in
			  Printf.printf ">>> %s\n" 
                    (substitute ["i",Printf.sprintf "%d" i] w)))
		  pg
		  w
		  table
		  (if !Opt.trace then
			(fun i j s ->
			  if m = 0 then
				print_string "[ ]"
			  else
				for k = 0 to m - 1 do
				  if k = j then
					begin
					  print_char '[';
					  print_char w.[k];
					  print_char ']'
					end
				  else
					begin
					  print_char ' ';
					  print_char w.[k];
					  print_char ' '
					end
				done;
			  Printf.printf "    pc=%-3d i=%-3d %-20s stack=%s\n" i j
				(if i < n then Opcodes.string_of_instruction string_of_int pg.Opcodes.code.(i) else "(end of program)")
				(String.concat "" (List.map (fun sym -> unoptstr (C.name_of values sym)) s)))
		  else
			(fun i j s -> ()))
	  in
	  begin
		match !Opt.run with
	  	  Some(w) ->
			begin
			  match process w with
				Opcodes.Automaton_accepts -> Printf.printf "Automaton accepts.\n"
			  | Opcodes.Automaton_rejects -> Printf.printf "Automaton rejects.\n"
			  | Opcodes.Automaton_is_halted(i,r) ->
				  Printf.printf "Automaton halted at instruction %d because %s.\n" i
					(match r with
					  Opcodes.Attempt_to_mark_out_of_bounds -> "it tried to mark beyond the bounds of the tape"
					| Opcodes.Attempt_to_mark_already_marked_case -> "it tried to mark an already marked case"
					| Opcodes.Attempt_to_move_head_beyond_limits -> "it tried to move the head off limits"
					| Opcodes.Attempt_to_pop_empty_stack -> "it couldn't pop from an empty stack"
					| Opcodes.Halt_instruction -> "of a HALT instruction")
			end
		| None -> ()
	  end;
	  if !Opt.filter then
		try
		  while true do
			let w = input_line stdin in
			if (match process w with Opcodes.Automaton_accepts -> true | _ -> false) <> !Opt.negate then
			  begin
				print_string w;
				print_char '\n'
			  end
		  done
		with
		  End_of_file -> ()
		  
let test fn = Compile.compile (parse_file fn)

let _ =
  Arg.parse
	Opt.specs
	(fun x -> ignore (process x))
	(Printf.sprintf "usage: %s [options] file" Sys.argv.(0));

