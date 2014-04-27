(* Boolean *)
(* $Id: boolean.ml,v 1.3 2002/10/11 12:05:21 durak Exp $ *)

type 'a t =
  Atom of 'a
| And of 'a t * 'a t
| Or of 'a t * 'a t
| Xor of 'a t * 'a t
| Not of 'a t
| True
| False

type 'a possibility = Always | Maybe of 'a | Never
type 'a evaluation = No | Depends of 'a | Yes

let simplify f x =
  let rec simplify f = function
	  Atom(a) as x ->
		begin
		  match f a with
			Always -> Yes
		  | Maybe(b) -> Depends(Atom(b))
		  | Never -> No
		end
	| And(p1,p2) ->
		begin
		  match simplify f p1, simplify f p2 with
			(No,_)|(_,No) -> No
		  | (Yes,x)|(x,Yes) -> x
		  | (Depends h1,Depends h2) -> Depends(And(h1,h2))
		end
	| Or(p1,p2) ->
		begin
		  match simplify f p1, simplify f p2 with
			(Yes,_)|(_,Yes) -> Yes
		  | (No,x)|(x,No) -> x
		  | (Depends h1,Depends h2) -> Depends(Or(h1,h2))
		end
	| Xor(p1,p2) ->
		begin
		  match simplify f p1, simplify f p2 with
			(Yes,Yes)|(No,No) -> No
		  | (Yes,No)|(No,Yes) -> Yes
		  | (Yes,Depends h)|(Depends h,Yes) -> Depends(Not(h))
		  | (x,No)|(No,x) -> x
		  | (Depends h1,Depends h2) -> Depends(Xor(h1,h2))
		end
	| Not(p) ->
		begin
		  match simplify f p with
			Yes -> No
		  | No -> Yes
		  | Depends h -> Depends(Not(h))
		end
	| True -> Yes
	| False -> No
  in
  match simplify f x with
	Yes -> True
  |	No -> False
  |	Depends(x) -> x

let rec eval f = function
	Atom(a) -> f a
  | And(p1,p2) -> (eval f p1) && (eval f p2)
  | Or(p1,p2) -> (eval f p1) || (eval f p2)
  | Not(p) -> not (eval f p)
  | Xor(p1,p2) -> (eval f p1) <> (eval f p2)
  | True -> true
  | False -> false

let rec fold f q = function
	Atom(a) -> f q a
  | (And(p1,p2)|Or(p1,p2)|Xor(p1,p2)) -> fold f (fold f q p1) p2
  | Not(p) -> fold f q p
  | True -> q
  | False -> q

let rec map f = function
	Atom(a) -> Atom(f a)
  | And(p1,p2) -> And(map f p1,map f p2)
  | Or(p1,p2) -> Or(map f p1,map f p2)
  | Not(p) -> Not(map f p)
  | Xor(p1,p2) -> Xor(map f p1,map f p2)
  | True -> True
  | False -> False

let sf = Printf.sprintf

let rec to_string f = function
	Atom(a) -> f a
  | And(p1,p2) -> "And("^(to_string f p1)^","^(to_string f p2)^")"
  | Or(p1,p2) -> "Or("^(to_string f p1)^","^(to_string f p2)^")"
  | Not(p) -> "Not("^(to_string f p)^")"
  | Xor(p1,p2) -> "Xor("^(to_string f p1)^","^(to_string f p2)^")"
  | True -> "True"
  | False -> "False"

let rec iter f = function
	Atom(a) -> f a
  | And(p1,p2) -> iter f p1; iter f p2
  | Or(p1,p2) -> iter f p1; iter f p2
  | Not(p) -> iter f p
  | Xor(p1,p2) -> iter f p1; iter f p2
  | True -> ()
  | False -> ()

