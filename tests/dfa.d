(* DFA *)
(* $Id: dfa.d,v 1.1 2003/05/27 12:48:06 durak Exp $ *)
(* *)

value a = "a"
value b = "b"
value q = "q"
value s = "s"
value t = "t"
value f = "f"
value comma = ","
value dash = "#"
value yes = "y"
value no = "n"

(* initial state is q *)
(* transitions are coded as *)
(* t q^i s^j q^k *)
(* input is coded as : *)
(* s^{i_1},s^{i_2},...,s^{i_k} *)
(* final states are coded as *)
(* fq^i *)

(* [transitions][final state]#[input-word] *)

procedure load_input_word_backwards =
  while not eof do right done;
  left;
  while not see dash do
    if see comma then push comma
    else if see s then push s
    else reject;
    left
  done

procedure rewind () = while not home do left done

procedure rec search =
  if (see q & top q) or (see s & top s) then
     begin
       pop ();
       right ()
     end
  else if (see q & top comma) then
     (* found correct transition *)
     begin
       while see q do left done;
       while see s do left done;
       right;
       while not see comma do
         if see q then push q
         else if see s then push s
         else reject
       done
     end
  else
    (* mismatch *)
    

procedure run_dfa =
  rewind ();
  search ()

procedure main =
  load_input_word_backwards ();
  push q;
  run_dfa ()

start main
