(* Moves *)

value a = "a"
value b = "b"
value sharp = "#"
value star = "*"

procedure divide_head_position_by_two =
  push sharp;
  while not home do
    left;
    if not home then
      begin
        push a;
        left
      end
    else ()
  done;
  while not top sharp do
    pop;
    right
  done;
  pop

procedure compute_logarithm_of_head_position =
  push star;
  while not home do
    divide_head_position_by_two ();
    push a
  done;
  while not top star do
    right;
    pop
  done;
  pop

procedure rewind =
  while not home do
    left
  done

(*procedure go_to_first x =
  rewind ();
  while not see x do
    right
  done*)

procedure go_to_first_b =
  rewind ();
  while not see b do
    right
  done

procedure main =
  go_to_first_b ();
  compute_logarithm_of_head_position ()
  
start main
