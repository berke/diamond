(* Cook *)
(* $Id: kmp.d,v 1.1 2003/06/12 11:14:34 durak Exp $ *)
(* vim:set ts=4: *)

value a = "a"
value b = "b"
value pound = "#"

(* utilities *)

procedure rewind = while not home do left done

procedure forward = while not eof do right done

procedure push_seen =
  if      see a then push a
  else if see b then push b
  else if see pound then push pound
  else
    begin
      if eof then
        output "push_seen called at eof"
      else
        output "push_seen called with unknown char";
      reject
    end

(* specifics *)

procedure copy_backwards_to_beginning =
  while not home do
    push_seen ();
    left
  done

procedure copy_backwards_to_pound =
  while not see pound do
    push_seen ();
    left
  done

procedure copy_to_eof =
  while not eof do
    push_seen ();
    right
  done

procedure search =
   forward (); left;
   copy_backwards_to_pound ();
   rewind ();
   while true do
     if see pound then
       begin
         output "String found.";
         accept
       end
     else
       if empty then
         begin
           output "String not found.";
           reject
         end
       else
         if (top a && see a) or (top b && see b) then
           begin
             pop;
             right
           end
         else
           (* mismatch, we must restore *)
           begin
             copy_backwards_to_beginning (); (* no need to pop one extra char beacuse home(0) is true *)
             pop
           end
   done

procedure main =
    output "This program accepts the language";
    output "{ u#vuw | u,v,w in (a+b)^* }.";
    output "It uses no WORM tape.";
    search ()

start main
