set_jrh_lexer;;

open Lib;;
open Fusion;;
open Printer;;

external str_list_fingerprint : string -> string list -> int list -> int = "TheoremFingerprint";;
(* Second argument is only supposed to be a string, but that somehow didn't
   work. Now we pass a string list of length 1. *)
(* external goal_int_fingerprint : int list -> string list -> int = "GoalFingerprint";; *)

(* Fingerprinting for terms that are not considered theorems in the ocaml     *)
(* typesystem, but of which we know that they are theorems.                   *)
(* USE WITH CARE!                                                             *)
let to_string = str_of_sexp o sexp_term;;
let term_fingerprint ((hyp_terms, concl): term list * term) : int =
  str_list_fingerprint (to_string concl) (List.map to_string hyp_terms) [];;

let fingerprint (th: thm) : int = term_fingerprint (dest_thm th);;

let goal_fingerprint ((assum, concl): thm list * term) : int =
  let assum_fps = List.map fingerprint assum in
  str_list_fingerprint ((str_of_sexp o sexp_term) concl) [] assum_fps;;


let theorem_index = Hashtbl.create 1000;;

let thm_is_known (th: thm) : bool = Hashtbl.mem theorem_index (fingerprint th);;

let thm_of_index (i: int) : thm = try
    Hashtbl.find theorem_index i
  with Not_found ->
    failwith ("No theorem exists with index " ^ string_of_int i);;

(* A theorem given index n can be referred to as "THM n" in a tactic
 * parameter *)
let index_thm (i: int) (thm: thm) : unit =
  (* Printf.eprintf "Registering THM %d\n%!" i; *)
  if Hashtbl.mem theorem_index i then
    (* Printf.eprintf
        "theorem_fingerprint.ml (index_thm): THM %d known already.\n%!" i; *)
    ()
  else
    Hashtbl.add theorem_index i thm;;

let register_thm thm : int =
  let fp = fingerprint thm in
  (if not (thm_is_known thm) then index_thm fp thm);
  fp;;
