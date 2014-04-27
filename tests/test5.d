(* *)

procedure main =
	if see "a" then
		begin
			right;
			if see "a" then
				accept
			else
				()
		end
	else
		reject

start main
