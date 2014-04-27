(* Twodpda *)
(* $Id: twodpda.ml,v 1.5 2003/07/25 16:38:54 durak Exp $ *)

open Pda

let output_pushdown_machine oc pda =
  Printf.fprintf oc "%d %d %d\n" pda.num_states pda.alphabet_size pda.stack_alphabet_size;
  for q = 0 to pda.num_states - 1 do
    for c = 0 to pda.alphabet_size + 1 do
      for t = 0 to pda.stack_alphabet_size - 1 do
        let (q',delta,operation) =
          pda.transition
            q
            (if c = 0 then `Left else if c = 1 then `Right else `Letter(c - 2))
            t
        in
        Printf.fprintf oc "# state %d input %d stack %d\n" q c t;
        match operation with
        | `Nop -> Printf.fprintf oc "%d %d 0\n" q' delta
        | `Pop -> Printf.fprintf oc "%d %d -1\n" q' delta
        | `Push t' -> Printf.fprintf oc "%d %d %d\n" q' delta (t' + 1)
      done
    done
  done

let lambda table q c t =
  table.(q).(match c with `Left -> 0 | `Right -> 1 | `Letter c -> c + 2).(t)

let comment_skipper_line_counter =
  let re = Str.regexp "[ \t]*\\([^ #][^#]*[^ #]\\|[^ #]\\)\\(#.*\\)?" in
  fun ic ->
    let ctr = ref 0 in
    let rec loop ic =
      incr ctr;
      let l = input_line ic in
      if Str.string_match re l 0 then
        Str.matched_group 1 l
      else
        loop ic
    in
    (loop, fun () -> !ctr)

let input_pushdown_machine ic =
  let (f,g) = comment_skipper_line_counter ic in
  try
    Scanf.sscanf (f ic) "%d %d %d"
      (fun num_states alphabet_size stack_alphabet_size ->
        let table = Array.init num_states
          (fun _ -> Array.init (alphabet_size + 2)
            (fun _ -> Array.make stack_alphabet_size (0,0,`Nop)))
        in
        for q = 0 to num_states - 1 do
          for c = 0 to alphabet_size + 1 do
            for t = 0 to stack_alphabet_size - 1 do
              Scanf.sscanf (f ic) "%d %d %d" (fun q' delta o ->
                table.(q).(c).(t) <-
                  (q',delta,
                   match o with
                   | -1 -> `Pop
                   | 0 -> `Nop
                   | _ -> `Push(o - 1)))
            done
          done
        done;
        { num_states = num_states;
          alphabet_size = alphabet_size;
          stack_alphabet_size = stack_alphabet_size;
          transition = lambda table })
   with
   | x ->
       Printf.eprintf "At line %d, exception %s.\n" (g ()) (Printexc.to_string x);
       raise x

let array_of_word w =
  let m = String.length w in
  Array.init m (fun i ->
    match w.[i] with
    | 'a'..'z' as c -> Char.code c - Char.code 'a'
    | _ -> failwith "invalid character")

let _ =
  if Array.length Sys.argv <> 3 then
    begin
      Printf.eprintf "usage: %s file.pda input-word\n"
        (Filename.basename Sys.argv.(0));
      exit 1
    end
  else
    begin
      let fn = Sys.argv.(1)
      and w = Sys.argv.(2)
      in
      let ic = open_in fn in
      let pda = input_pushdown_machine ic in
      close_in ic;
      (* Printf.eprintf "PDA follows:\n";
      output_pushdown_machine stderr pda;
      flush stderr; *)
      Simulator.simulate pda (array_of_word (String.make (int_of_string w) 'a'))
    end

