(* Test1 *)
(* $Id: test1.d,v 1.1 2002/10/15 14:35:11 durak Exp $ *)

tape tape1
value value1 = "zozo"

procedure rec rewind =
	while not home do
		push value1;
		left
	done;
	rewind ()

procedure rec main =
	right;
	if not eof then
		main ()
	else
		begin
			rewind ();
			left;
			main ()
		end

start rewind
