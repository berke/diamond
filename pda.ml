(* PDA *)
(* $Id: pda.ml,v 1.1 2003/06/17 10:49:27 durak Exp $ *)

type t = {
  num_states : int; (* nombre d'états *)
  alphabet_size : int; (* taille de l'alphabet d'entrée *)
  stack_alphabet_size : int; (* taille de l'alphabet de pile *)
  transition : (* fonction de transition *)
     int -> (* état de contrôle *)
     [`Left|`Right|`Letter of int] -> (* symbole sur la bande de lecture ou délimiteurs *)
     int -> (* symbole de haut de pile *)
     (* résultat *)
     (int * (* nouvel état de contrôle *)
      int * (* entier relatif précisant le mouvement de la tête *)
      [`Pop (* dépiler *)
      |`Push of int (* empiler le symbole de pile donné *)
      |`Nop] (* ne pas toucher à la pile *)
      )
}

let sf = Printf.sprintf
let debug x = Printf.eprintf "debug: %s\n" x; flush stderr
