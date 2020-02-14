(* These tests correspond to testFingerprintEqualToOCamlImplementation
   in third_party/deepmath/deephol/theorem_fingerprint_test.py                *)

set_jrh_lexer;;
open Lib;;
open Fusion;;
open Printer;;
open Pb_printer;;
open Test_common;;


let true_term = Parser.parse_term "T";;
let false_term = Parser.parse_term "F";;

let theorems = [
  Sets.IN_UNION;  (* theorem without GEN%PVARS and ?types *)
  Sets.INSERT;    (* theorem with GEN%PVAR *)
  ASSUME true_term;
  Bool.ADD_ASSUM false_term (ASSUME true_term); (* multiple hypotheses *)
  ];;
let expected_fingerprints = [975856962495483397; 345492714299286815; 4420649969775231556; 2661800747689726299];;

(* The following comments are helpful for debugging: *)
(* printf "%s\n%!" (Printer.str_of_sexp (Printer.sexp_term Fingerprint_tests.true_term));; *)
(* printf "%s\n%!" (Printer.str_of_sexp (Printer.sexp_thm (Fusion.ASSUME Fingerprint_tests.true_term)));; *)
(* printf "%s\n%!" (Printer.str_of_sexp (Printer.sexp_thm (Bool.ADD_ASSUM Fingerprint_tests.false_term (Fusion.ASSUME Fingerprint_tests.true_term))));; *)

let fingerprint_matches (theorem, expected_fp: thm * int) : unit =
  let thm_fp = Theorem_fingerprint.fingerprint theorem in
  assert_equal_int thm_fp expected_fp;;

register_tests "Fingerprints match expected values."
  fingerprint_matches (zip theorems expected_fingerprints);;

let term_fingerprint_matches ((t, expected_fp): term * int) : unit =
  let term_fp : int = Theorem_fingerprint.term_fingerprint ([], t) in
  assert_equal_int term_fp expected_fp;;

register_tests "Fingerprint of term matches expected value."
  term_fingerprint_matches [(true_term, 70761607289060832)];;


let goal_fingerprint_matches ((goal, expected_fp): (thm list * term) * int) : unit =
  let goal_fp : int = Theorem_fingerprint.goal_fingerprint goal in
  assert_equal_int goal_fp expected_fp;;

register_tests "Fingerprint of goal matches expected value."
  goal_fingerprint_matches [(([ASSUME true_term], true_term), 4196583489440000546)];;
