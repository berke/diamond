(* Palindrome *)
(* $Id: palindrome.d,v 1.8 2002/10/16 14:41:46 durak Exp $ *)

value a = "a"
value b = "b"

procedure rewind =
	while not home do
		left
	done

procedure copy =
	while not eof do
		if see a then push a else push b;
		right
	done

procedure check =
	while not empty do
		if see a && top a or see b && top b then
			begin
				pop;
				right
			end
		else
			reject
	done;
	accept

procedure main =
	output "Copying.";
	copy ();
	output "Rewinding.";
	rewind ();
	output "Checking.";
	check ()

start main
