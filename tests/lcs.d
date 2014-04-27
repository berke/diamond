(* LCS *)
(* $Id: exponential.d,v 1.1 2003/06/17 11:38:11 durak Exp $ *)
(* vim:set ts=4: *)

value a = "a"
value b = "b"
value dash = "#"
value zero = "0"
value one = "1"

procedure rewind = while not home do left done

procedure left_half = rewind ()

procedure right_half =
  rewind ();
  while not see dash do right done;
  right

procedure rec next_word =
  rewind ();
  while top b do
    pop; right
  done;
  if empty then
    begin
      if home then
        accept
      else
        left;
        while not home do
          push a; left
        done
    end
  else
    begin
      pop; push b;
      while not home do
        push a;
        left
      done
    end

procedure rec search_stack_in_input () =
  if empty then
    push "found"
  else
    if (top a && see a) or (top b & see b) then
      begin
        pop;
        right;
        search_stack_in_input ()
      end
    else
      begin
        while something do (* argh ! *)
          left;
          if see a then push a
          else if see b then push b
        done
      end
      
procedure load_length =
  while not eof do
    push a; right
  done

procedure main =
  load_length ();
  while true do
    output "toto";
    next_word ()
  done

start main
