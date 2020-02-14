set_jrh_lexer;;
open Lib;;
open Fusion;;
open Printer;;


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
