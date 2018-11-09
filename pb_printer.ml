
set_jrh_lexer;;
open System;;
open Lib;;
open Fusion;;
open Basics;;
open Printer;;

(* ---------------------------------------------------------------------------*)
(* Protobuf printer functions.                                                *)
(* ---------------------------------------------------------------------------*)
let print_sexp_pb_field
    (fmt: Format.formatter) (field: string) (sexp: sexp) : unit =
  pp_print_string fmt (" " ^ field ^ ": \"");
  sexp_print fmt sexp;
  pp_print_string fmt "\"";;

let print_int_pb (fmt: Format.formatter) (field_name: string) i : unit =
  pp_print_string fmt (Printf.sprintf (" %s: %d ") field_name i)

let print_thm_pb (fmt: Format.formatter)
    ((assumptions, conclusion): term list * term) (tag: string)
    (part: data_partition option) (def_type: string option)
    (def_term: term option) (constants: string list)
    (thm_arg: thm option): unit =
  List.iter
      (fun asm ->
        print_sexp_pb_field fmt "hypotheses" (sexp_term asm))
      assumptions;
  print_sexp_pb_field fmt " conclusion" (sexp_term conclusion);
  pp_print_string fmt (" tag: " ^ tag);
  (match part with
    None -> ()
  | Some Test -> pp_print_string fmt (" training_split: TESTING ")
  | Some Valid -> pp_print_string fmt (" training_split: VALIDATION ")
  | Some Train -> pp_print_string fmt (" training_split: TRAINING "));
  match tag with
    "DEFINITION" -> (
      pp_print_string fmt " definition {";
      (match def_type with
        None -> ()
      | Some definition_type ->
          pp_print_string fmt (" definition_type: \"" ^ definition_type ^ "\""));
      (match def_term with
        None -> ()
      | Some term ->
          print_sexp_pb_field fmt " definition_term" (sexp_term term));
      List.iter
        (fun c -> pp_print_string fmt (" constants: \"" ^ c ^ "\"")) constants;
      (match thm_arg with
        None -> ()
      | Some thm_arg -> print_int_pb fmt "theorem_arg"
          (Theorem_fingerprint.fingerprint thm_arg));
      pp_print_string fmt "}")
  | _ -> ();;

(* ---------------------------------------------------------------------------*)
(* Print functions for theorem database.                                      *)
(*                                                                            *)
(* Code cannot move to log.ml as it is needed for very early definitions.     *)
(* ---------------------------------------------------------------------------*)
let thm_db_print_definition (log: bool) (definition_type: string) (theorem: thm)
    (term: term) (recursion_thm: thm option) (constants: (string*hol_type) list)
    : unit =
  if not log then () else
  match thm_db_fmt with
    Some fmt ->
      pp_print_string fmt "theorems {";
      print_int_pb fmt "fingerprint" (Theorem_fingerprint.fingerprint theorem);
      print_thm_pb fmt (dest_thm theorem) "DEFINITION" None (Some definition_type)
          (Some term) (map fst constants) recursion_thm;
      pp_print_string fmt "}\n";
      Format.pp_print_flush fmt ()
  | None -> ();;

let thm_db_print_theorem (thm_tuple: term list * term)
    (part: data_partition option) =
  match thm_db_fmt with
    Some fmt ->
      pp_print_string fmt "theorems {";
      print_int_pb fmt "fingerprint"
          (Theorem_fingerprint.term_fingerprint thm_tuple);
      print_thm_pb fmt thm_tuple "THEOREM" part None None [] None;
      pp_print_string fmt "}\n";
      Format.pp_print_flush fmt ()
  | None -> ();;

let thm_db_print_specification (log: bool)
     (definition_type: string) (constants: string list)
     (thm_arg: thm) (resulting_theorem: thm) : unit =
  if not log then () else
  match thm_db_fmt with
    Some fmt ->
      pp_print_string fmt "theorems {";
      print_int_pb fmt
          "fingerprint" (Theorem_fingerprint.fingerprint resulting_theorem);
      print_thm_pb fmt (dest_thm resulting_theorem) "DEFINITION" None
          (Some definition_type) None constants (Some thm_arg);
      pp_print_string fmt "}\n";
      Format.pp_print_flush fmt ()
  | None -> ();;
