(* Cook *)
(* $Id: cook.d,v 1.1 2002/10/16 15:28:20 durak Exp $ *)

value a = "a"
value b = "b"
value x = "x"

procedure rewind =
	while not home do
		left
	done

procedure push_seen =
	if see a then push a else if see b then push b else push x

procedure copy =
	while not eof do
		push_seen ();
		right
	done

procedure pop_and_check_acceptance odd message =
	if empty then reject else pop;
	if not home && empty && odd = "yes" then
		begin
			output message;
			accept
		end
	else ()

procedure rec check odd =
	output "Check";
	output odd;
	if see a && top a or see b && top b or see x && top x then
		begin
			pop_and_check_acceptance odd "first";
			right;
			if odd = "yes" then check "no" else check "yes"
		end
	else
		begin
			output "Discrepancy found, restoring.";
			while not home do
				left;
				push_seen ()
			done;
			pop_and_check_acceptance odd "second";
			check "no"
		end

procedure main =
	if home && eof then
		reject
	else
		begin
			output "Copying.";
			copy ();
			output "Rewinding.";
			rewind ();
			output "Checking.";
			check "no"
		end

start main
