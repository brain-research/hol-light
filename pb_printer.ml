
set_jrh_lexer;;
open System;;
open Lib;;
open Fusion;;
open Basics;;
open Printer;;

let library_tags = ref ["core"];;

let pb_print_library_tags fmt : unit =
  List.iter (fun library ->
    pp_print_string fmt (" library_tag: \"" ^ library ^ "\""))
    !library_tags;;

let escape_backslashes : string -> string =
  Str.global_replace (Str.regexp "\\\\") "\\\\\\\\";;

let pp_print_string fmt s = pp_print_string fmt (escape_backslashes s);;

let pb_string_of_thm (th: thm) : string =
  let th_string = string_of_thm th in
  let no_newlines = Str.global_replace (Str.regexp "\n") " " th_string in
  Str.global_replace (Str.regexp "  ") " " no_newlines;;

(* ---------------------------------------------------------------------------*)
(* Protobuf printer functions.                                                *)
(* ---------------------------------------------------------------------------*)
let print_sexp_pb_field
    (fmt: Format.formatter) (field: string) (sexp: sexp) : unit =
  pp_print_string fmt (" " ^ field ^ ": \"");
  pp_print_string fmt (str_of_sexp sexp);
  pp_print_string fmt "\"";;

let print_int_pb (fmt: Format.formatter) (field_name: string) i : unit =
  pp_print_string fmt (Printf.sprintf (" %s: %d ") field_name i)

let print_thm_pb (fmt: Format.formatter)
    ((assumptions, conclusion): term list * term) (tag: string)
    (part: data_partition option)
    (definition_printer : Format.formatter -> unit) : unit =
  print_int_pb fmt "fingerprint" (Theorem_fingerprint.term_fingerprint (assumptions, conclusion));
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
      definition_printer fmt;
      pp_print_string fmt "}")
  | "TYPE_DEFINITION" -> (
      pp_print_string fmt " type_definition {";
      definition_printer fmt;
      pp_print_string fmt "}")
  | _ -> ();;

(* ---------------------------------------------------------------------------*)
(* Print functions for theorem database.                                      *)
(*                                                                            *)
(* Code cannot move to log.ml as it is needed for very early definitions.     *)
(* ---------------------------------------------------------------------------*)

let print_definition
    (definition_type: string) (term: term option)
    (recursion_thm: thm option) (constants: string list)
    : Format.formatter -> unit =
  fun fmt ->
      pp_print_string fmt (" definition_type: \"" ^ definition_type ^ "\"");
      (match term with
        None -> ()
      | Some term -> print_sexp_pb_field fmt " definition_term" (sexp_term term));
      List.iter
        (fun c -> pp_print_string fmt (" constants: \"" ^ c ^ "\"")) constants;
      (match recursion_thm with
        None -> ()
      | Some recursion_thm -> print_int_pb fmt "theorem_arg"
          (Theorem_fingerprint.fingerprint recursion_thm));;

let thm_db_print_definition (log: bool) (definition_type: string) (th: thm)
    (term: term) (recursion_thm: thm option) (constants: (string*hol_type) list)
    : unit =
  if not log then () else
  match thm_db_fmt with
    Some fmt ->
      pp_print_string fmt "theorems {";
      print_thm_pb fmt (dest_thm th) "DEFINITION" None
        (print_definition definition_type (Some term) recursion_thm (map fst constants));
      pp_print_string fmt (" pretty_printed: \"" ^ pb_string_of_thm th ^ "\"");
      pb_print_library_tags fmt;
      pp_print_string fmt "}\n";
      Format.pp_print_flush fmt ()
  | None -> ();;

let print_type_definition (tyname: string) (absname: string) (repname: string)
    (th_arg: thm) : Format.formatter -> unit =
  fun fmt ->
      pp_print_string fmt (" type_name: \"" ^ tyname ^ "\"");
      pp_print_string fmt (" abs_name: \"" ^ absname ^ "\"");
      pp_print_string fmt (" rep_name: \"" ^ repname ^ "\"");
      print_int_pb fmt "theorem_arg" (Theorem_fingerprint.fingerprint th_arg);;

let thm_db_print_type_definition (tyname: string)
    (absname: string) (repname: string) (th_arg: thm) (th_result: thm) : unit =
  match thm_db_fmt with
    Some fmt ->
      pp_print_string fmt "theorems {";
      pp_print_string fmt (" pretty_printed: \"" ^ pb_string_of_thm th_result ^ "\"");
      print_thm_pb fmt (dest_thm th_result) "TYPE_DEFINITION" None
          (print_type_definition tyname absname repname th_arg);
      pp_print_string fmt "}\n";
      Format.pp_print_flush fmt ()
  | None -> ();;

let thm_db_print_theorem (th: thm) (part: data_partition option)
    (source: string) : unit =
  match thm_db_fmt with
    Some fmt ->
      pp_print_string fmt "theorems {";
      print_thm_pb fmt (dest_thm th) "THEOREM" part (fun _ -> ());
      pp_print_string fmt (" pretty_printed: \"" ^ pb_string_of_thm th ^ "\"");
      pb_print_library_tags fmt;
      pp_print_string fmt (" proof_function: \"" ^ source ^ "\"");
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
      print_thm_pb fmt (dest_thm resulting_theorem) "DEFINITION" None
          (print_definition definition_type None (Some thm_arg) constants);
      pp_print_string fmt (" pretty_printed: \"" ^ pb_string_of_thm resulting_theorem ^ "\"");
      pb_print_library_tags fmt;
      pp_print_string fmt "}\n";
      Format.pp_print_flush fmt ()
  | None -> ();;
