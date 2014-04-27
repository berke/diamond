(* Palpref *)
(* $Id: palpref.d,v 1.1 2003/07/25 16:40:39 durak Exp $ *)
(* Calcule le plus long préfixe palindromique de son entrée. *)
(* Testé par vérification exhaustive sur les mots de longueur 10 et 11 *)

value a = "a"
value b = "b"

procedure push_seen =
  if see a then push a
  else if see b then push b
  else
    begin
      output "illegal input";
      reject
    end

procedure rewind = while not home do left done

procedure rec palpref =
  if empty then
    begin
      output "longest prefix has length $i";
      accept
    end
  else
    if top a && see a or top b && see b then
      begin
        pop;
        right;
        palpref ()
      end
    else
      begin
        while not home do
          left;
          push_seen ()
        done;
        pop;
        palpref ()
      end

procedure main = 
  while not eof do push_seen (); right done;
  rewind ();
  palpref ()

start main
