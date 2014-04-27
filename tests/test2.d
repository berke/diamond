(* Test2 *)
(* $Id: test2.d,v 1.1 2002/10/15 14:35:11 durak Exp $ *)

tape worm1

procedure rec rewind =
	while not home do
		left
	done

procedure rec forward =
	while not eof do
		right
	done

procedure push_seen =
	if see "a" then
		push "a"
	else
		push "b"

procedure rec main =
	push_seen ();
	right;
	if not eof then
		main ()
	else
		begin
			rewind ();
			loop1 ()
		end
and loop1 =
	output "loop1";
	if top ">" then
		begin
			left;
			mark worm1 with "1";
			if home then
				begin
					forward ();
					start2 ()
				end
			else
				restore1 ()
		end
	else
		()
and continue1 =
	pop;
	right;
	loop1 ()
and restore1 =
	left;
	if home then
		continue1 ()
	else
		begin
			push_seen ();
			restore1 ()
		end
and start2 =
	push_seen ();
	left;
	push "gogol";
	if not home then
		start2 ()
	else
		begin
			forward ();
			left;
			left;
			loop2 ()
		end
and loop2 =
	if top "<" then
		if worm1 marked with "1" then
			accept
		else
			if eof then
				reject
			else
				restore2 ()
	else
		if top ">" && see ">" or top "<" && see "<" then
			continue2 ()
		else
			restore2 ()
and continue2 =
	pop;
	left;
	loop2 ()
and restore2 =
	right;
	if eof then
		begin
			left;
			continue2 ()
		end	
	else
		begin
			push_seen ();
			restore2 ()
		end

start main
