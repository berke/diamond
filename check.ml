(* Check *)
(* $Id: check.ml,v 1.4 2002/10/16 15:28:16 durak Exp $ *)

open Ast
open Intermediate

module C = Catalog.Make(String)

exception Invalid_recursion of string * string

let rec is_empty = function
	[] -> true
  |	x::y ->
	  (match x with
		`Sequence(r) -> is_empty r
	  |	_ -> false) && (is_empty y)

let f i = function None -> Printf.sprintf "(%d)" i | Some x -> Printf.sprintf "%s(%d)" x i

let check_recursion procedures start =
  let m = C.cardinality procedures in
  let r = Array.make m [] (* recursive calls *)
  and t = Array.make m [] (* tail calls *)
  in
  let add p l = if List.mem p l then l else p::l in
  let search i =
	let rec f has_rest = function
		`Sequence([])::rest -> f has_rest rest
	  |	`Sequence(_::_ as l)::rest -> f has_rest (l@rest)
	  |	`If(_,st1,st2)::rest ->
		  let x = has_rest or (not (is_empty rest)) in
		  f x [st1];
		  f x [st2];
		  f has_rest rest
	  |	`Op(_)::rest -> f has_rest rest
	  |	`Apply(pid,_)::rest ->
		  if has_rest or not (is_empty rest) then
			begin
			  r.(i) <- add pid r.(i);
			  f has_rest rest
			end
		  else
			t.(i) <- add pid t.(i)
	  |	[] -> ()
	in
	f false [(C.find procedures i).body]
  in
  for i = 0 to m - 1 do
	search i
  done;
  let c = Array.make_matrix m m false in
  for i = 0 to m - 1 do
	List.iter (fun j -> c.(i).(j) <- true) r.(i);
	List.iter (fun j -> c.(i).(j) <- true) t.(i);
  done;

  if !Opt.dump_call_graph then
	begin
	  Printf.printf "Call graph before FW:\n";
	  for i = 0 to m - 1 do
		Printf.printf "\tProcedure %-3d calls :" i;
		for j = 0 to m - 1 do
		  if c.(i).(j) then Printf.printf " %d" j;
		done;
		Printf.printf "\n";
		
		Printf.printf "\tProcedure %-3d calls recursively :" i;
		List.iter (fun j -> Printf.printf " %d" j) r.(i);
		Printf.printf "\n";
	  done
	end;

  (* transitive closure *)
  for i = 1 to m - 1 do
	(* ajout du sommet n *)
	for j = 0 to i - 1 do
	  for k = 0 to i - 1 do
		if c.(j).(i) && c.(i).(k) then c.(j).(k) <- true
	  done
	done
  done;

  if !Opt.dump_call_graph then
	begin
	  Printf.printf "Call graph after FW:\n";
	  for i = 0 to m - 1 do
		Printf.printf "\tProcedure %-3d calls :" i;
		for j = 0 to m - 1 do
		  if c.(i).(j) then Printf.printf " %d" j;
		done;
		Printf.printf "\n";
		
		Printf.printf "\tProcedure %-3d calls recursively :" i;
		List.iter (fun j -> Printf.printf " %d" j) r.(i);
		Printf.printf "\n";
	  done
	end;

  (* check *)
  for i = 0 to m - 1 do
	List.iter (fun j ->
	  (* procedure i calls procedure j recursively *)
	  if c.(j).(i) then
		(* procedure j calls procedure i in one way or another -- no good *)
		raise (Invalid_recursion(f i (C.name_of procedures i),f j (C.name_of procedures j)))) r.(i)
  done
