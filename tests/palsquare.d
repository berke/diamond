(* Longest even-length palindromic prefix *)

value a = "a"
value b = "b"
value sharp = "#"

procedure rewind = while not home do left done
procedure forward = while not eof do right done

procedure rec lpp2 =
  if top sharp then
    if home then
      reject
    else
      begin
        left;
        if home then
          reject
        else
          pop
      end
  else
    if see a && top a or see b && top b then
      begin
        pop; right;
        lpp2 ()
      end
    else
      begin
        while not home do
          if see a then push a else push b;
          left
        done;
        pop;
        lpp2 ()
      end

procedure rec lpp =
  push sharp;
  while not eof do
    if see a then push a else push b;
    right
  done;
  rewind ();
  lpp2 ()

procedure palsquare =
  forward ();
  while not home do
    left;
    if see a then push a else push b
  done;
  lpp ();
  output "palprefix OK at $i";
  while not home do
    pop; left
  done;
  pop;
  forward ();
  left;
  while not empty do
    if see a && top a or see b && top b then
      begin
        pop; left
      end
    else
      reject
  done;
  accept

start palsquare
