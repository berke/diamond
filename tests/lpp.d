(* Longest even-length palindromic prefix *)

value a = "a"
value b = "b"
value sharp = "#"

procedure rewind = while not home do left done

procedure rec lpp =
  push sharp;
  while not eof do
    if see a then push a else push b;
    right
  done;
  output "rewind";
  rewind ();
  lpp2 ()
and lpp2 =
  if top sharp then
    if home then
      reject
    else
      begin
        left;
        if home then
          reject
        else
          accept
      end
  else
    if see a && top a or see b && top b then
      begin
        output "match";
        pop; right;
        lpp2 ()
      end
    else
      begin
        output "mismatch";
        while not home do
          if see a then push a else push b;
          left
        done;
        pop;
        pop;
        lpp2 ()
      end

start lpp
