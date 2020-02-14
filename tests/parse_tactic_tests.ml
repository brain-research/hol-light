(* Integration tests for parse_tactic and parse_rule. *)

set_jrh_lexer;;
open Lib;;
open Fusion;;
open Parser;;
open Printer;;
open Pb_printer;;
open Tactics;;
open Simp;;
open Log;;
open Parse_tactic;;
open Test_common;;

Printer.current_encoding := Printer.Pretty;;

let true_term = Parser.parse_term "T";;
let false_term = Parser.parse_term "F";;

let theorems = [
  Sets.IN_UNION;  (* 975856962495483397; theorem without GEN%PVARS and ?types *)
  Sets.INSERT;    (* 345492714299286815; theorem with GEN%PVAR *)
  ASSUME true_term;
  Bool.ADD_ASSUM false_term (ASSUME true_term); (* multiple hypotheses *)
  Bool.IMP_DEF;
  Sets.FORALL_IN_UNION;  (* 4052081339122143671 *)
  ASSUME `p /\ q`;  (* 3653885712938130508 *)
  ASSUME `f = ((\x y. x + y) y)`;  (* 1748915323273844750 *)
  Equal.SYM(ASSUME `x:num = y`);  (* 237159312591742290  *)
  Equal.SYM(ASSUME `y:num = x`);  (* 3739319715096271711 *)
  Bool.SPEC_ALL Arith.GT; (* 1636085460980557391 *)
  Class.TAUT `p /\ q ==> q /\ p`; (* 3093559110319623167 *)
  Class.TAUT `q /\ p ==> p /\ q`; (* 262723677586854601 *)
  Nums.INFINITY_AX; (* 650638948164601243 *)
];;
List.iter (fun t -> Theorem_fingerprint.register_thm t; ()) theorems;;
let string_of_fp = string_of_int o Theorem_fingerprint.fingerprint;;
let theorem_fingerprints = "[ THM " ^ (String.concat " ; THM "
  (map string_of_fp theorems)) ^ " ]";;
printf "%s%!" theorem_fingerprints;;

let asm_goals: goal list = [
    ([("", ASSUME `p:bool`)], `p:bool`);
]

let goals: goal list = asm_goals @ [
    ([], `p`);
    ([], `x = x`);
    ([], `y = y /\ x = x`);
    ([], `y = y ==> x = x`);
    ([], `!P s t:A->bool.
          (!x. x IN s UNION t ==> P x) <=>
          (!x. x IN s ==> P x) /\ (!x. x IN t ==> P x)`); (* FORALL_IN_UNION *)
  ];;

(* Test parsing with assumptions from the assumption list *)
let asm_string_tactics: string list = [
    "REWRITE_TAC [ ASSUM 0 ]";
];;
let asm_expected_tactics: tactic list = [
    Simp.ASM_REWRITE_TAC [];
];;

(* Test tactic parsing for tactics with/without theorem lists *)
let string_tactics: string list = [
    "CONJ_TAC ";
    "DISCH_TAC ";
    "SIMP_TAC [ ]";
    "MESON_TAC [ ]";
    "MP_TAC THM 975856962495483397 ";
    "REWRITE_TAC [ ]";
    "REWRITE_TAC [ THM 975856962495483397 ]";
    "REWRITE_TAC [ THM 345492714299286815 ]";
    "REWRITE_TAC " ^ theorem_fingerprints;
];;
let expected_tactics: tactic list = [
    CONJ_TAC;
    DISCH_TAC;
    SIMP_TAC [];
    Meson.MESON_TAC [];
    MP_TAC Sets.IN_UNION;
    Simp.REWRITE_TAC [];
    Simp.REWRITE_TAC [Sets.IN_UNION];
    Simp.REWRITE_TAC [Sets.INSERT];
    Simp.REWRITE_TAC theorems;
];;

(* Turn tactic applications into sexpressions, catch tactic failures*)
let try_tactic (t:tactic): goal -> string = (fun g ->
  try
    let _, goals, _: goalstate = t g in
      String.concat "; " (map (str_of_sexp o sexp_goal) goals)
  with Failure f -> "Failure: " ^ f);;

let assert_same_tactic_behavior_goals
  (goals:goal list) (actual, expected: tactic * tactic): unit =
    let res_actual = map (try_tactic actual) goals in
    let res_expected = map (try_tactic expected) goals in
      assert_equal_list res_actual res_expected;;

let assert_same_tactic_behavior = assert_same_tactic_behavior_goals goals;;
let asm_assert_same_tactic_behavior =
  assert_same_tactic_behavior_goals asm_goals;;

let actual_tactics = map Parse_tactic.parse string_tactics in
  register_tests "Parsed tactics exhibit expected behavior on set of goals."
    assert_same_tactic_behavior (zip actual_tactics expected_tactics);;

let actual_tactics = map Parse_tactic.parse asm_string_tactics in
  register_tests
    "Parsed tactics with asl arguments exhibit expected behavior."
    asm_assert_same_tactic_behavior (zip actual_tactics asm_expected_tactics);;


(* Tests for parse_rule *)
let input_output : (string * thm) list = [
    "REWRITE_RULE [ THM 975856962495483397 ] THM 4052081339122143671",
    REWRITE_RULE [Sets.IN_UNION] Sets.FORALL_IN_UNION;
    "ARITH_RULE `x = 1 ==> y <= 1 \/ x < y`",
    Ints.ARITH_RULE `x = 1 ==> y <= 1 \/ x < y`;
    "ASM_REWRITE_RULE [ ] THM 3653885712938130508",
    ASM_REWRITE_RULE [] (ASSUME `p /\ q`);
    "BETA_RULE THM 1748915323273844750",
    Equal.BETA_RULE (ASSUME `f = ((\x y. x + y) y)`);
    "CONJ_ACI_RULE `(a /\ b) /\ (a /\ c) <=> (a /\ (c /\ a)) /\ b`",
    Canon.CONJ_ACI_RULE `(a /\ b) /\ (a /\ c) <=> (a /\ (c /\ a)) /\ b`;
    "DEDUCT_ANTISYM_RULE THM 237159312591742290 THM 3739319715096271711",
    DEDUCT_ANTISYM_RULE (Equal.SYM(ASSUME `x:num = y`)) (Equal.SYM(ASSUME `y:num = x`));
    "DISJ_ACI_RULE `(p \/ q) \/ (q \/ r) <=> r \/ q \/ p`",
    Canon.DISJ_ACI_RULE `(p \/ q) \/ (q \/ r) <=> r \/ q \/ p`;
    "EQ_IMP_RULE_LEFT THM 1636085460980557391",
    fst (Bool.EQ_IMP_RULE (Bool.SPEC_ALL Arith.GT));
    "EQ_IMP_RULE_RIGHT THM 1636085460980557391",
    snd (Bool.EQ_IMP_RULE (Bool.SPEC_ALL Arith.GT));
    "IMP_ANTISYM_RULE THM 3093559110319623167 THM 262723677586854601",
    Bool.IMP_ANTISYM_RULE (Class.TAUT `p /\ q ==> q /\ p`) (Class.TAUT `q /\ p ==> p /\ q`);
    "INTEGER_RULE `!x y n:int. (x == y) (mod n) ==> (n divides x <=> n divides y)`",
    Ints.INTEGER_RULE `!x y n:int. (x == y) (mod n) ==> (n divides x <=> n divides y)`;
    "NUMBER_RULE `!a b a' b'. ~(gcd(a,b) = 0) /\ a = a' * gcd(a,b) /\ b = b' * gcd(a,b) ==> coprime(a',b')`",
    Ints.NUMBER_RULE
     `!a b a' b'. ~(gcd(a,b) = 0) /\ a = a' * gcd(a,b) /\ b = b' * gcd(a,b)
                  ==> coprime(a',b')`;
    "ONCE_ASM_REWRITE_RULE " ^ theorem_fingerprints ^ " THM 975856962495483397",
    ONCE_ASM_REWRITE_RULE theorems Sets.IN_UNION;
    "ONCE_REWRITE_RULE " ^ theorem_fingerprints ^ " THM 345492714299286815",
    ONCE_REWRITE_RULE theorems Sets.INSERT;
    "ONCE_SIMP_RULE " ^ theorem_fingerprints ^ " THM 4052081339122143671",
    ONCE_SIMP_RULE theorems Sets.FORALL_IN_UNION;
    "PURE_ASM_REWRITE_RULE " ^ theorem_fingerprints ^ " THM 3653885712938130508",
    PURE_ASM_REWRITE_RULE theorems (ASSUME `p /\ q`);
    "PURE_ONCE_ASM_REWRITE_RULE " ^ theorem_fingerprints ^ " THM 1748915323273844750",
    PURE_ONCE_ASM_REWRITE_RULE theorems (ASSUME `f = ((\x y. x + y) y)`);
    "PURE_ONCE_REWRITE_RULE " ^ theorem_fingerprints ^ " THM 237159312591742290",
    PURE_ONCE_REWRITE_RULE theorems (Equal.SYM(ASSUME `x:num = y`));
    "PURE_REWRITE_RULE " ^ theorem_fingerprints ^ " THM 1636085460980557391",
    PURE_REWRITE_RULE theorems (Bool.SPEC_ALL Arith.GT);
    "PURE_SIMP_RULE [ ] THM 3093559110319623167",
    PURE_SIMP_RULE [] (Class.TAUT `p /\ q ==> q /\ p`);
    "REWRITE_RULE " ^ theorem_fingerprints ^ " THM 3093559110319623167",
    REWRITE_RULE theorems (Class.TAUT `p /\ q ==> q /\ p`);
    "SELECT_RULE THM 650638948164601243",
    Class.SELECT_RULE (Nums.INFINITY_AX);
    "SET_RULE `{x | ~(x IN s <=> x IN t)} = (s DIFF t) UNION (t DIFF s)`",
    Sets.SET_RULE `{x | ~(x IN s <=> x IN t)} = (s DIFF t) UNION (t DIFF s)`;
    "SIMP_RULE " ^ theorem_fingerprints ^ " THM 3093559110319623167",
    SIMP_RULE theorems (Class.TAUT `p /\ q ==> q /\ p`);
    "SPEC_ALL THM 4052081339122143671",
    Bool.SPEC_ALL Sets.FORALL_IN_UNION;
    "CONJ THM 3093559110319623167 THM 262723677586854601",
    Bool.CONJ (Class.TAUT `p /\ q ==> q /\ p`) (Class.TAUT `q /\ p ==> p /\ q`);
    "CONJUNCT1 THM 3653885712938130508",
    Bool.CONJUNCT1 (ASSUME `p /\ q`);
    "CONJUNCT2 THM 3653885712938130508",
    Bool.CONJUNCT2 (ASSUME `p /\ q`);
    "DISCH_ALL THM 2661800747689726299",
    Bool.DISCH_ALL (Bool.ADD_ASSUM false_term (ASSUME true_term));
    "MATCH_MP THM 3093559110319623167 THM 3653885712938130508",
    Drule.MATCH_MP (Class.TAUT `p /\ q ==> q /\ p`) (ASSUME `p /\ q`);
    "MP THM 3093559110319623167 THM 3653885712938130508",
    Bool.MP (Class.TAUT `p /\ q ==> q /\ p`) (ASSUME `p /\ q`);
    "GEN_ALL THM 1636085460980557391",
    Bool.GEN_ALL (Bool.SPEC_ALL Arith.GT);
    "GSYM THM 237159312591742290",
    Equal.GSYM (Equal.SYM(ASSUME `x:num = y`));
];;

let results: (thm * thm) list =
  map (fun s, th -> (Parse_tactic.parse_rule s, th)) input_output in
  let assert_equal_theorem_pair (th1, th2) = assert_normalized_equal_theorems th1 th2 in
    register_tests "Parsed rules result in equal output theorems."
    assert_equal_theorem_pair results;;

