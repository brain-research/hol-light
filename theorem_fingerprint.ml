set_jrh_lexer;;

open Lib
open Fusion
open Printer

external str_list_fingerprint : string list -> int = "TheoremFingerprint";;

(* Fingerprinting for terms that are not considered theorems in the ocaml     *)
(* typesystem, but of which we know that they are theorems.                   *)
(* USE WITH CARE!                                                             *)
let term_fingerprint ((hyp_terms, term): term list * term) =
  str_list_fingerprint (List.map (str_of_sexp o sexp_term) (term::hyp_terms))

let fingerprint (th: thm) = term_fingerprint (dest_thm th)


let theorem_index = Hashtbl.create 1000
let thm_is_known (th: thm) : bool = Hashtbl.mem theorem_index (fingerprint th)
let thm_of_index (i: int) : thm = try
    Hashtbl.find theorem_index i
  with Not_found ->
    failwith ("No theorem exists with index " ^ string_of_int i)

(* A theorem given index n can be referred to as "THM n" in a tactic
 * parameter *)
let index_thm (i: int) (thm: thm) : unit =
  (* Printf.eprintf "Registering THM %d\n%!" i; *)
  if Hashtbl.mem theorem_index i then
    (* Printf.eprintf
        "theorem_fingerprint.ml (index_thm): THM %d known already.\n%!" i; *)
    ()
  else
    Hashtbl.add theorem_index i thm

let register_thm thm : unit =
  index_thm (fingerprint thm) thm;;
