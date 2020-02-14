
set_jrh_lexer;;
Pb_printer.set_file_tags ["import_proofs.ml"];;

open Lib;;
open Fusion;;
open Log;;

(* This file contains infrastructure for importing the proofs from DeepHOL *)

Printexc.record_backtrace true;;

type wrapped_proof = unit -> goal * tactic;;

(* Statistics *)
let num_proofs_replaced = ref 0;;
let num_replaced_proofs_succeeded = ref 0;;
let num_replaced_proofs_failed = ref 0;;

(* Hashtable mapping goal fingerprints to lambda-wrapped tactics *)
let proof_index = Hashtbl.create 1000;;
(* Also remember which proofs are not in core; these must eventually be
   requested. *)
let has_to_be_checked = Hashtbl.create 1000;;

(* Fingerprint for goal objects based on the normalized conclusion. *)
let goal_fingerprint (g: goal) : int =
  let asl, (concl: term) = g in
  (if length asl > 0 then
    failwith "Cannot handle assumptions for goal fingerprints");
  Theorem_fingerprint.term_fingerprint ([], Normalize.normalize_term concl);;

let proof_index_contains (g: goal) : bool =
  Hashtbl.mem proof_index (goal_fingerprint g);;

let proof_of_goal (g: goal) : wrapped_proof =
  let goal_fp = goal_fingerprint g in
  try
    let res = Hashtbl.find proof_index goal_fp in
    Printf.printf "Importing proof with goal_fp %d\n%!" goal_fp;
    Hashtbl.remove has_to_be_checked goal_fp;
    res
  with Not_found ->
    failwith ("No proof found for goal with index " ^ string_of_int goal_fp);;

(* Call this function to 'import' a proof. *)
let register_proof (goal_fp: int) (t: wrapped_proof) (in_core: bool) : unit =
  (* Printf.eprintf "register_proof for %d\n%!" goal_fp; *)
  if Hashtbl.mem proof_index goal_fp then
    Printf.printf "Error in register_proof: tried to register two proofs for fingerprint %d for the same goal.\n\!" goal_fp
  else
    Hashtbl.add proof_index goal_fp t;
    if not in_core then Hashtbl.add has_to_be_checked goal_fp ();;

let process_hyps asms =
  map (fun asm -> ("", Fusion.ASSUME (Parser.decode_term asm))) asms;;

let decode_goal asms concl = process_hyps asms, Parser.decode_term concl;;

Pb_printer.clear_file_tags();;
