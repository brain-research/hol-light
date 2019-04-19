
set_jrh_lexer;;
open Lib;;
open Fusion;;
open Printer;;

let library_tags = ref ["core"];;
let file_tags = ref None;;
let is_some x =
  match x with
    None -> false
  | Some _ -> true;;
let set_file_tags tags =
  (if is_some !file_tags then failwith "file tag is already set");
  library_tags := List.append !library_tags tags;
  file_tags := Some tags;;
let clear_file_tags () =
  match !file_tags with
    None -> failwith ("file tag is not set")
  | Some tags ->
      library_tags := filter (fun s -> not (List.memq s tags)) !library_tags;
  file_tags := None;;

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
(* Normalization of GEN%PVARs                                                 *)
(* ---------------------------------------------------------------------------*)

let is_genpvar_name (var_name: string) : bool =
  let re = Str.regexp "GEN%PVAR%[0-9]+" in (* matches vars like GEN%PVAR%8342 *)
  Str.string_match re var_name 0;;

let is_genpvar (tm: term) : bool =
  is_var tm && (is_genpvar_name o fst o dest_var) tm;;

let is_genpvar_abstraction (tm: term) : bool =
  is_abs tm && (is_genpvar o fst o dest_abs) tm;;

(* Traverse term bottom up; create and combine conversion to rename GEN%PVARs *)
let rec normalize_genpvars_conv (nesting: int) (tm: term) : Equal.conv =
  match tm with
      Var(s,ty) ->
        REFL  (* = ALL_CONV *)
    | Const(s,ty) ->
        REFL  (* = ALL_CONV *)
    | Comb(l,r) ->
        Equal.COMB2_CONV
          (normalize_genpvars_conv nesting l)
          (normalize_genpvars_conv nesting r)
    | Abs(var,body) ->
        if is_genpvar var
        then
          let body_conv = normalize_genpvars_conv (nesting+1) body in
          let rename_conv = Equal.ALPHA_CONV
            (mk_var ("GEN%PVAR%" ^ string_of_int nesting, type_of var)) in
          Equal.EVERY_CONV [Equal.ABS_CONV body_conv; rename_conv]
        else
          Equal.ABS_CONV (normalize_genpvars_conv nesting body);;

let normalize_genpvars_conv (tm: term) : Equal.conv =
  normalize_genpvars_conv 0 tm;;

let assert_no_hypotheses (th: thm) : unit =
  if List.length (Fusion.hyp th) != 0 then
    failwith (
      Printf.sprintf
        "Theorem with hypotheses encountered during normalization: %s"
        (str_of_sexp (sexp_thm th))
      )
  else ();;

let normalize_genpvars (th: thm) : thm =
  Equal.CONV_RULE (normalize_genpvars_conv (concl th)) th;;

let normalize_genpvars_in_term (tm: term) : term =
  let conversion_theorem = normalize_genpvars_conv tm tm in
  assert_no_hypotheses conversion_theorem;
  (snd o dest_eq o concl) conversion_theorem;;

(* ---------------------------------------------------------------------------*)
(* Normalization of generic types                                             *)
(* ---------------------------------------------------------------------------*)

let is_gen_tvar_name (tvar_name: string) : bool =
  let re = Str.regexp "\\?[0-9]+" in (* matches types of the form ?928342 *)
  Str.string_match re tvar_name 0;;

let is_gen_tvar tvar = (is_gen_tvar_name o dest_vartype) tvar;;

let normalizing_type_substitutions (tms : term list) :
    (hol_type * hol_type) list =
  let tvars = remove_duplicates__stable
    (List.concat (map type_vars_in_term__stable tms)) in
  let gen_tvars = filter is_gen_tvar tvars in
  List.mapi
    (fun idx tvar -> mk_vartype ("?" ^ string_of_int idx), tvar)
    gen_tvars;;

(* Instantiates types of the form ?<large_number> by ?<canonical_number>. *)
let normalize_generic_type_variables (th: thm) : thm =
  let hyps, concl = dest_thm th in
  INST_TYPE (normalizing_type_substitutions (concl::hyps)) th;;

let normalize_generic_type_variables_terms (tms: term list) : term list =
  let substitutions = normalizing_type_substitutions tms in
  map (inst substitutions) tms;;

(* ---------------------------------------------------------------------------*)
(* Normalization functions for terms and theorems.                            *)
(* ---------------------------------------------------------------------------*)

let normalize_terms (tms: term list) : term list =
  normalize_generic_type_variables_terms (map normalize_genpvars_in_term tms);;

let normalize_term (tm: term) : term = hd (normalize_terms [tm]);;

let normalize_theorem (th: thm) : thm =
  normalize_generic_type_variables (normalize_genpvars th);;

(* ---------------------------------------------------------------------------*)
(* Protobuf printer functions                                                 *)
(* ---------------------------------------------------------------------------*)
let print_sexp_pb_field
    (fmt: Format.formatter) (field: string) (sexp: sexp) : unit =
  pp_print_string fmt (" " ^ field ^ ": \"");
  pp_print_string fmt (str_of_sexp sexp);
  pp_print_string fmt "\"";;

let print_int_pb (fmt: Format.formatter) (field_name: string) i : unit =
  pp_print_string fmt (Printf.sprintf (" %s: %d ") field_name i)

let print_goal_pb (fmt: Format.formatter)
    ((assumptions, conclusion): term list * term) (tag: string)
    (definition_printer : Format.formatter -> unit) : unit =
  let conclusion::assumptions = normalize_terms (conclusion::assumptions) in
  print_int_pb fmt "fingerprint"
      (Theorem_fingerprint.term_fingerprint (assumptions, conclusion));
  List.iter
      (fun asm ->
        print_sexp_pb_field fmt "hypotheses" (sexp_term asm))
      assumptions;
  print_sexp_pb_field fmt " conclusion" (sexp_term conclusion);
  pp_print_string fmt (" tag: " ^ tag);
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

let print_thm_pb (fmt: Format.formatter)
    (th:  thm) (tag: string)
    (definition_printer : Format.formatter -> unit) : unit =
  print_goal_pb fmt (dest_thm th) tag definition_printer;;

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
  let th = normalize_theorem th in
  Theorem_fingerprint.register_thm th;
  if not log then () else
  match thm_db_fmt with
    Some fmt ->
      let term = normalize_term term in
      pp_print_string fmt "theorems {";
      print_thm_pb fmt th "DEFINITION"
        (print_definition
            definition_type (Some term) recursion_thm (map fst constants));
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
  let th_result = normalize_theorem th_result in
  Theorem_fingerprint.register_thm th_result;
  match thm_db_fmt with
    Some fmt ->
      pp_print_string fmt "theorems {";
      pp_print_string fmt (" pretty_printed: \"" ^ pb_string_of_thm th_result ^ "\"");
      print_thm_pb fmt th_result "TYPE_DEFINITION"
          (print_type_definition tyname absname repname th_arg);
      pb_print_library_tags fmt;
      pp_print_string fmt "}\n";
      Format.pp_print_flush fmt ()
  | None -> ();;

let thm_db_print_theorem (th: thm)
    (source: string) (goal_fingerprint : int option) : unit =
  let th = normalize_theorem th in
  if not (Theorem_fingerprint.thm_is_known th) then (
  Theorem_fingerprint.register_thm th;
  match thm_db_fmt with
    Some fmt ->
      pp_print_string fmt "theorems {";
      print_thm_pb fmt th "THEOREM" (fun _ -> ());
      pp_print_string fmt (" pretty_printed: \"" ^ pb_string_of_thm th ^ "\"");
      pb_print_library_tags fmt;
      pp_print_string fmt (" proof_function: \"" ^ source ^ "\"");
      (match goal_fingerprint with
        Some goal_fingerprint ->
          print_int_pb fmt "goal_fingerprint" goal_fingerprint;
      | None -> ());
      pp_print_string fmt "}\n";
      Format.pp_print_flush fmt ()
  | None -> ());;

let thm_db_print_specification (log: bool)
     (definition_type: string) (constants: string list)
     (thm_arg: thm) (th: thm) : unit =
  let thm_arg = normalize_theorem thm_arg in
  thm_db_print_theorem thm_arg "specification" None;
  let th = normalize_theorem th in
  Theorem_fingerprint.register_thm th;
  if not log then () else
  match thm_db_fmt with
    Some fmt ->
      pp_print_string fmt "theorems {";
      print_thm_pb fmt th "DEFINITION"
          (print_definition definition_type None (Some thm_arg) constants);
      pp_print_string fmt (" pretty_printed: \"" ^ pb_string_of_thm th ^ "\"");
      pb_print_library_tags fmt;
      pp_print_string fmt "}\n";
      Format.pp_print_flush fmt ()
  | None -> ();;
