(* PDA *)
(* $Id: pda.ml,v 1.1 2003/06/17 10:49:27 durak Exp $ *)

type t = {
  num_states : int; (* nombre d'�tats *)
  alphabet_size : int; (* taille de l'alphabet d'entr�e *)
  stack_alphabet_size : int; (* taille de l'alphabet de pile *)
  transition : (* fonction de transition *)
     int -> (* �tat de contr�le *)
     [`Left|`Right|`Letter of int] -> (* symbole sur la bande de lecture ou d�limiteurs *)
     int -> (* symbole de haut de pile *)
     (* r�sultat *)
     (int * (* nouvel �tat de contr�le *)
      int * (* entier relatif pr�cisant le mouvement de la t�te *)
      [`Pop (* d�piler *)
      |`Push of int (* empiler le symbole de pile donn� *)
      |`Nop] (* ne pas toucher � la pile *)
      )
}

let sf = Printf.sprintf
let debug x = Printf.eprintf "debug: %s\n" x; flush stderr
