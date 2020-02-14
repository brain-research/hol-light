set_jrh_lexer;;
open Lib;;
open Fusion;;
open Tactics;;
open Equal;;
open Simp;;
open Theorem_fingerprint;;

module Parse : sig
  val parse : string -> tactic;;
  val parse_rule : string -> thm;;
end =
struct
  type data = string * int
  type env = thm list
  let empty (s, i) = i >= String.length s
  let str_of_data (d : data) = Printf.sprintf ":%d" (snd d)
  exception Pop_failure
  let pop (s, i) w =
    let ls, lw = String.length s, String.length w in
    if i + lw <= ls && String.sub s i lw = w && (
      i + lw = ls || s.[i + lw] = ' ')
    then (s, i + lw + 1) else raise Pop_failure
  let pop_word (s, i) =
    let j = try
      String.index_from s i ' '
    with Not_found -> String.length s in
    (String.sub s i (j-i), (s, j+1))

  type 'a parser = env -> data -> 'a * data
  let apply abp ap (e : env) (d : data) =
    let (ab, d1) = abp e d in let (a, d2) = ap e d1 in (ab a, d2)
  let const a (e : env) (d : data) = (a, d)
  let fail d want got = failwith (
    Printf.sprintf "Parse failure at %s: expected %s but found %s"
    (str_of_data d) want got)
  let check_end d want =
    if empty d then fail d want "end of input" else ()
  let fn v c = c (const v)
  let applyc abp ap c = c (apply abp ap)

  let listparser ap (e : env) (d : data) =
    let rec get_a l d1 =
      let a, d2 = ap e d1 in
      let l1, (w, d3) = a::l, pop_word d2 in
      if w = "]" then
        (List.rev l1, d3)
      else if w = ";" then
        get_a l1 d3
      else fail d2 "] or ;" w in
    let w, d1 = pop_word d in
    if w = "[" then
      try
        let d2 = pop d1 "]" in ([], d2)
      with Pop_failure -> get_a [] d1
    else fail d "[" w

  let add_new h k v =
    try
      let _ = Hashtbl.find h k in
      failwith ("Key '"^k^"' already exists")
    with Not_found -> Hashtbl.add h k v
  let keyword_parser typename =
    let h = Hashtbl.create 1000 in
    let add_word (p, w) = add_new h w (p I) in
    let add_words = List.iter add_word in
    let parse e d =
      check_end d typename;
      let (w, d1) = pop_word d in
      try
        let p = Hashtbl.find h w in (p e d1)
      with Not_found -> fail d typename w in
    (parse, add_words)

  let (thp : thm parser), add_th = keyword_parser "thm"
  let (tcp : tactic parser), add_tc = keyword_parser "tactic"
  let (rule_p : thm parser), add_rule = keyword_parser "rule"
  let (term_rule_p : (term -> thm) parser), add_term_rule = keyword_parser "term_rule"
  let (convp : conv parser), add_conv = keyword_parser "conv"
  let (convfnp : (conv -> conv) parser), add_convfn = keyword_parser "convfn"

  let (intp : int parser) = fun e d ->
    check_end d "int";
    let (w, d1) = pop_word d in
    (int_of_string w, d1)
  let (tmp : term parser) =
    let delim = '`' in
    fun e (s, i) ->
      check_end (s, i) "term";
      if s.[i] != delim then
        fail (s, i) "`" (String.sub s i 1)
      else
        let j = try
          String.index_from s (i+1) delim
        with Not_found ->
          fail (s, i) "matching `" "end of input" in
        let tm = String.sub s (i+1) (j-i-1) in
        (Parser.decode_term tm, (s, j+2))

  let th p = applyc p thp
  let thl p c = applyc p (listparser thp) c
  let conv p c = applyc p convp c
  let convfn p c = applyc p convfnp c
  let convfnl p c = applyc p (listparser convfnp) c
  let tm p c = applyc p tmp c
  let tmtm p c = applyc p (fn (curry I) tm tm I) c
  let n p c = applyc p intp c
  let assum (e : env) (d : data) = (List.nth e, d)

  let () = add_th [
    n assum, "ASSUM";
  ]

  let () = add_tc [
    fn ABS_TAC, "ABS_TAC";
    fn ACCEPT_TAC th, "ACCEPT_TAC";
    fn ANTS_TAC, "ANTS_TAC";
    fn Ints.ARITH_TAC, "ARITH_TAC";
    fn Meson.ASM_MESON_TAC thl, "ASM_MESON_TAC";
    fn Metis.ASM_METIS_TAC thl, "ASM_METIS_TAC";
    fn Metis.METIS_TAC thl, "METIS_TAC";
    fn Ind_defs.BACKCHAIN_TAC th, "BACKCHAIN_TAC";
    fn CHEAT_TAC, "CHEAT_TAC";
    fn CHOOSE_TAC th, "CHOOSE_TAC";
    fn CONJ_TAC, "CONJ_TAC";
    fn CONTR_TAC th, "CONTR_TAC";
    fn (CONV_TAC " ") conv, "CONV_TAC";
    fn DISCH_TAC, "DISCH_TAC";
    fn DISJ1_TAC, "DISJ1_TAC";
    fn DISJ2_TAC, "DISJ2_TAC";
    fn DISJ_CASES_TAC th, "DISJ_CASES_TAC";
    fn EQ_TAC, "EQ_TAC";
    fn EXISTS_TAC tm, "EXISTS_TAC";
    fn GEN_TAC, "GEN_TAC";
    fn (Simp.GEN_REWRITE_TAC " ") convfn thl, "GEN_REWRITE_TAC";
    fn Itab.ITAUT_TAC, "ITAUT_TAC";
    fn MATCH_ACCEPT_TAC th, "MATCH_ACCEPT_TAC";
    fn MATCH_MP_TAC th, "MATCH_MP_TAC";
    fn Meson.MESON_TAC thl, "MESON_TAC";
    fn MK_COMB_TAC, "MK_COMB_TAC";
    fn MP_TAC th, "MP_TAC";
    fn Simp.ONCE_REWRITE_TAC thl, "ONCE_REWRITE_TAC";
    fn Simp.PURE_ONCE_REWRITE_TAC thl, "PURE_ONCE_REWRITE_TAC";
    fn Simp.PURE_REWRITE_TAC thl, "PURE_REWRITE_TAC";
    fn RAW_CONJUNCTS_TAC th, "RAW_CONJUNCTS_TAC";
    fn RAW_POP_ALL_TAC, "RAW_POP_ALL_TAC";
    fn RAW_POP_TAC n, "RAW_POP_TAC";
    fn RAW_SUBGOAL_TAC tm, "RAW_SUBGOAL_TAC";
    fn Calc_rat.REAL_ARITH_TAC, "REAL_ARITH_TAC";
    fn Reals.REAL_ARITH_TAC, "REAL_ARITH_TAC2";
    fn REFL_TAC, "REFL_TAC";
    fn Simp.REWRITE_TAC thl, "REWRITE_TAC";
    fn Simp.SIMP_TAC thl, "SIMP_TAC";
    fn SPEC_TAC tmtm, "SPEC_TAC";
    fn SUBST1_TAC th, "SUBST1_TAC";
    fn TRANS_TAC th tm, "TRANS_TAC";
    fn UNDISCH_TAC tm, "UNDISCH_TAC";
    fn X_CHOOSE_TAC tm th, "X_CHOOSE_TAC";
    fn X_GEN_TAC tm, "X_GEN_TAC";
  ]
  let EQ_IMP_RULE_LEFT = (fun th ->
    fst (Bool.EQ_IMP_RULE th))
  let EQ_IMP_RULE_RIGHT = (fun th ->
    snd (Bool.EQ_IMP_RULE th))
  let () = add_rule [
    (* A rule produces a thm from various arguments, usually another thm. *)
    fn ASM_REWRITE_RULE thl th, "ASM_REWRITE_RULE";
    fn BETA_RULE th, "BETA_RULE";
    fn CONV_RULE conv th, "CONV_RULE";
    fn DEDUCT_ANTISYM_RULE th th, "DEDUCT_ANTISYM_RULE";
    (* EQ_IMP_RULE thm->thm*thm so split into left and right. *)
    fn EQ_IMP_RULE_LEFT th, "EQ_IMP_RULE_LEFT";
    fn EQ_IMP_RULE_RIGHT th, "EQ_IMP_RULE_RIGHT";
    fn Bool.IMP_ANTISYM_RULE th th, "IMP_ANTISYM_RULE";
    fn ONCE_ASM_REWRITE_RULE thl th, "ONCE_ASM_REWRITE_RULE";
    fn ONCE_REWRITE_RULE thl th, "ONCE_REWRITE_RULE";
    fn ONCE_SIMP_RULE thl th, "ONCE_SIMP_RULE";
    fn PURE_ASM_REWRITE_RULE thl th, "PURE_ASM_REWRITE_RULE";
    fn PURE_ONCE_ASM_REWRITE_RULE thl th, "PURE_ONCE_ASM_REWRITE_RULE";
    fn PURE_ONCE_REWRITE_RULE thl th, "PURE_ONCE_REWRITE_RULE";
    fn PURE_REWRITE_RULE thl th, "PURE_REWRITE_RULE";
    fn PURE_SIMP_RULE thl th, "PURE_SIMP_RULE";
    fn REWRITE_RULE thl th, "REWRITE_RULE";
    fn Class.SELECT_RULE th, "SELECT_RULE";
    fn SIMP_RULE thl th, "SIMP_RULE";
    (* Other rule-like transformations, but not named *_RULE *)
    fn Bool.SPEC_ALL th, "SPEC_ALL";
    fn Bool.CONJ th th, "CONJ";
    fn Bool.CONJUNCT1 th, "CONJUNCT1";
    fn Bool.CONJUNCT2 th, "CONJUNCT2";
    fn Bool.DISCH_ALL th, "DISCH_ALL";
    fn Bool.DISJ_CASES th th th, "DISJ_CASES";
    fn Bool.EQF_ELIM th, "EQF_ELIM";
    fn Bool.EQF_INTRO th, "EQF_INTRO";
    fn Bool.EQT_ELIM th, "EQT_ELIM";
    fn Bool.EQT_INTRO th, "EQT_INTRO";
    fn Fusion.EQ_MP th th, "EQ_MP";
    fn Bool.EXISTENCE th, "EXISTENCE";
    fn Bool.GEN_ALL th, "GEN_ALL";
    fn Equal.GSYM th, "GSYM";
    fn Bool.IMP_TRANS th th, "IMP_TRANS";
    fn Ints.INT_OF_REAL_THM th, "INT_OF_REAL_THM";
    fn Arith.LE_IMP th, "LE_IMP";
    fn Drule.MATCH_MP th th, "MATCH_MP";
    fn Drule.MK_CONJ th th, "MK_CONJ";
    fn Drule.MK_DISJ th th, "MK_DISJ";
    fn Bool.MP th th, "MP";
    fn Bool.NOT_ELIM th, "NOT_ELIM";
    fn Bool.NOT_INTRO th, "NOT_INTRO";
    fn Bool.PROVE_HYP th th, "PROVE_HYP";
    fn Reals.REAL_LET_IMP th, "REAL_LET_IMP";
    fn Reals.REAL_LE_IMP th, "REAL_LE_IMP";
    fn Bool.SIMPLE_DISJ_CASES th th, "SIMPLE_DISJ_CASES";
    fn Equal.SUBS thl th, "SUBS";
    fn Equal.SYM th, "SYM";
    fn Fusion.TRANS th th, "TRANS";
    fn Bool.UNDISCH th, "UNDISCH";
    fn Bool.UNDISCH_ALL th, "UNDISCH_ALL";
    (* RULES that require a term argument *)
    (* NOTE: these are only the term rules that end _RULE *)
    fn Ints.ARITH_RULE tm, "ARITH_RULE";
    fn Canon.CONJ_ACI_RULE tm, "CONJ_ACI_RULE";
    fn Canon.DISJ_ACI_RULE tm, "DISJ_ACI_RULE";
    fn Ints.INTEGER_RULE tm, "INTEGER_RULE";
    fn Ints.NUMBER_RULE tm, "NUMBER_RULE";
    fn Sets.SET_RULE tm, "SET_RULE";
  ]
  let () = add_conv [
    fn BETA_CONV, "BETA_CONV";
    fn Class.CONTRAPOS_CONV, "CONTRAPOS_CONV";
    fn Pair.GEN_BETA_CONV, "GEN_BETA_CONV";
    fn Calc_num.NUM_REDUCE_CONV, "NUM_REDUCE_CONV";
    fn Calc_rat.REAL_RAT_REDUCE_CONV, "REAL_RAT_REDUCE_CONV";
    fn SYM_CONV, "SYM_CONV";
    fn I convfn conv, "APPLY";
  ]
  let composel (l : (conv -> conv) list) = List.fold_left (o) I l
  let () = add_convfn [
    fn BINDER_CONV, "BINDER_CONV";
    fn BINOP_CONV, "BINOP_CONV";
    fn LAND_CONV, "LAND_CONV";
    fn ONCE_DEPTH_CONV, "ONCE_DEPTH_CONV";
    fn RAND_CONV, "RAND_CONV";
    fn RATOR_CONV, "RATOR_CONV";
    fn REDEPTH_CONV, "REDEPTH_CONV";
    fn TOP_DEPTH_CONV, "TOP_DEPTH_CONV";
    fn composel convfnl, "COMPOSE";
  ]

  let return_if_empty retv d =
    if empty d then retv else
      let (s1, i) = d in
      let remainder = String.sub s1 i (String.length s1 - i) in
      fail d "end of input" remainder

  let parse s = ASSUM_LIST (fun thl ->
    let (tc, d) = tcp thl (s, 0) in
    return_if_empty tc d)

  let () = add_th [fn Theorem_fingerprint.thm_of_index n, "THM"]

  (* A named theorem can be referred to by name in a tactic parameter *)
  let name_thm name theorem = add_th [fn theorem, name]

  (* Parse strings into corresponding rules *)
  let parse_rule (s:string):thm =
    (* Piggy-backing on generic parser, which expects asl as env.
       passing empty list for env, ASSUM params are always illegal *)
    let (rulec, d) = rule_p [] (s, 0) in
    return_if_empty rulec d

end
include Parse
