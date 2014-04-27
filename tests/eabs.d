(* Eabs *)
(* $Id: eabs.d,v 1.2 2003/05/27 12:48:06 durak Exp $ *)
(* Recognizes { u#v | forall k  u in (1+(a+b)^*b)a^k(1+b(a+b)^* ) => v in (1+(a+b)^*b)a^k(1+b)(a+b)^* } *)

value a = "a"
value b = "b"
value dash = "#"
value yes = "y"
value no = "n"

procedure skip_to_next_block =
  while not (see b or see dash) do
    right
  done;
  right

procedure restore =
  left;
  while not (see b or home or see dash) do
    push a;
    left
  done;
  if (home or see dash) && see a then push a else ()

procedure rec search_integer_in_stack_in_current_half =
  if not empty && (see dash or eof) then
    begin
      restore ();
      push no
    end
  else
    begin
      while not empty && see a do pop; right done;
      if empty && not see a then
         begin
           restore ();
           push yes
         end
      else
        begin
          restore ();
          right;
          while see a do
            right
          done;
          if see b then
            begin
              right;
              search_integer_in_stack_in_current_half ()
            end
          else
            push no
        end
    end

procedure load_all_a_s =
  while not see dash do
     push a;
     right
  done
  
procedure rewind = while not home do left done

procedure skip_to_dash = while not see dash do right done

procedure left_half = rewind ()

procedure right_half = rewind (); skip_to_dash (); right

procedure main =
  load_all_a_s ();
  while not empty do
    left_half ();
    search_integer_in_stack_in_current_half ();
    if top yes then
      begin
        pop;
        right_half ();
        search_integer_in_stack_in_current_half ();
        if top yes then
           begin
             pop;
             pop
           end
        else reject
      end
    else
      begin
        pop;
        pop
      end
  done;
  accept

start main
