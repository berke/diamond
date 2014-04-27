(* Exponential *)
(* $Id: exponential.d,v 1.1 2003/06/17 11:38:11 durak Exp $ *)
(* vim:set ts=4: *)

value a = "a"
value b = "b"

procedure rewind = while not home do left done

procedure rec increment =
  output "increment";
  while top b && not eof do
    pop; right
  done;
  if eof then
    accept
  else
    begin
      pop; push b;
      while not home do
        push a;
        left
      done;
      increment ()
    end

procedure main =
  while not eof do
    push a; right
  done;
  rewind ();
  increment ()

start main
