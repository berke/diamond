(* Moves *)

value a = "a"
value b = "b"
value sharp = "#"
value star = "*"
value yes = "y"
value no = "n"
value zero = "0"
value one = "1"

procedure rewind =
  while not home do
    left
  done

procedure restore =
  rewind ();
  while not top star do
    right;
    pop
  done;
  pop

procedure rec is_odd =
  if home then
    push no
  else
    begin
      push a;
      left;
      is_even ()
    end
and is_even =
  if home then
    push yes
  else
    begin
      push a;
      left;
      is_odd ()
    end

procedure parity =
  push star;
  is_even ();
  if top yes then
    begin
      pop;
      restore ();
      push zero
    end
  else
    begin
      pop;
      restore ();
      push one
    end

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

procedure save =
  push sharp;
  while not home do
    push a;
    left
  done

procedure restore_double =
  while not top sharp do
    pop;
    right;
    right
  done;
  pop

procedure double_head_position =
  save ();
  restore_double ()

procedure restore_binary =
  rewind ();
  while not top star do
    double_head_position ();
    if top one then
      begin
        right
      end
    else ();
    pop
  done;
  pop

procedure save_binary =
  push star;
  while not home do
    parity ();
    divide_head_position_by_two ()
  done

procedure go_to_first x =
  rewind ();
  while not see x do
    right
  done

procedure go_to_first_b =
  rewind ();
  while not see b do
    right
  done

procedure main =
  go_to_first_b ();
  save_binary ();
  restore_binary ()

start main
