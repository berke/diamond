(* Palindrome *)
(* $Id: test3.d,v 1.1 2002/10/15 14:35:11 durak Exp $ *)

tape bok

value gogol = "zozo"

tape got
tape gogol

procedure rewind =
	while not home do
		left
	done	

procedure x = left
and y = right
and z = left ; push "toto" ; right ; while true do left done
and push_twice x = push x ; push x
and push_if_equal x y z =
	if x = y then
		push z
	else
		right

value z = "trente-trois"

procedure rec gogol c =
	push c;
	left;
	toto z
and toto x =
	if top x then
		begin
			push "hello";
			gogol "a"
		end
	else
		begin
			pop;
			output "now going to eat this...";
			gogol "z"
		end

procedure test x =
	push x

procedure rec zorgol x y z =
	test x;
	zorgol x y z

procedure push_a_lot_of_times a b c d x y z =
	while not home do
		if top x then
			push y
		else
			push z
	done

procedure main =
	while true do
		push_a_lot_of_times "a" "b" "c" "d" "x" "y" "z"
	done	

start main
