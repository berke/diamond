(* Simulator *)
(* $Id: simulator.ml,v 1.2 2003/07/25 16:38:54 durak Exp $ *)

open Pda

exception Looping of int * int * int

let simulate pda word =
  let m = Array.length word in
  let encode q i stack_symbol = q + pda.num_states * (i + (m + 2) * stack_symbol)
  in
  let n = (pda.num_states * (m + 2) * pda.stack_alphabet_size) in
  let accel = Array.make n None
  and active = Array.make n false
  in
  let rec simulate q i t =
    (* debug (sf "simulate q = %d i = %d t = %d" q i t); *)
    let s = encode q i t in
      match accel.(s) with
      | Some(q',i') -> (q',i')
      | None ->
         loop q i t []
  and loop q i t visited =
    let s = encode q i t in
    if active.(s) then
      raise (Looping(q,i,t))
    else
      active.(s) <- true;
    (* debug (sf "loop q = %d i = %d t = %d visited = [%s]" q i t
      (String.concat ";" (List.map (fun (q,i) -> sf "%d,%d" q i) visited))); *)
    let visited' = (q,i)::visited in
    let (q',move,stack_operation) =
      pda.transition
        q
        (if i = 0 then `Left
         else if i = m + 1 then `Right
         else `Letter word.(i - 1))
        t
    in
    let i' = max 0 (min (m + 1) (i + move)) in
    match stack_operation with
    | `Nop ->
        loop q' i' t visited'
    | `Push t' ->
        let (q'',i'') = simulate q' i' t' in
        loop q'' i'' t visited'
    | `Pop ->
        List.iter (fun (q'',i'') ->
          let s = encode q'' i'' t in
          accel.(s) <- Some(q',i');
          active.(s) <- false)
          visited';
        (q',i')
  in
  simulate 0 0 0
