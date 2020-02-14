set_jrh_lexer;;
open Lib;;
open Fusion;;
open Printer;;
open Pb_printer;;
open Test_common;;
open Normalize;;

let idempotent_term_normalization : term -> unit =
  assert_idempotent assert_equal_terms normalize_term;;

let idempotent_theorem_normalization =
  assert_idempotent assert_equal_theorems normalize_theorem;;

let theorems = [
  Sets.IN_UNION;  (* theorem without GEN%PVARS and ?types *)
  Sets.INSERT;    (* theorem with GEN%PVAR *)
  ];;
let terms = map concl theorems;;

register_tests "Theorem normalization is idempotent"
  idempotent_theorem_normalization theorems;;

register_tests "Term normalization is idempotent"
  idempotent_term_normalization terms;;

current_encoding := Sexp;;

let normalize_sexp : string -> string =
  str_of_sexp o sexp_term o normalize_term o Parser.decode_term;;

let examples = [
  ((str_of_sexp o sexp_term o normalize_term o concl) Sets.INSERT,
    "(a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) =) (a (a (c "^
    "(fun A (fun (fun A (bool)) (fun A (bool)))) INSERT) (v A x)) (v (fun A "^
    "(bool)) s))) (a (c (fun (fun A (bool)) (fun A (bool))) GSPEC) (l (v A GEN"^
    "%PVAR%0) (a (c (fun (fun A (bool)) (bool)) ?) (l (v A y) (a (a (a (c (fun"^
    " A (fun (bool) (fun A (bool)))) SETSPEC) (v A GEN%PVAR%0)) (a (a (c (fun "^
    "(bool) (fun (bool) (bool))) \\/) (a (a (c (fun A (fun (fun A (bool)) (boo"^
    "l))) IN) (v A y)) (v (fun A (bool)) s))) (a (a (c (fun A (fun A (bool))) "^
    "=) (v A y)) (v A x)))) (v A y)))))))");
  (normalize_sexp "(v ?42 x)", "(v ?0 x)");
  (normalize_sexp "(v ?42 GEN%PVAR%100)", "(v ?0 GEN%PVAR%100)");
  (normalize_sexp "(l (v ?42 GEN%PVAR%100) (v ?42 GEN%PVAR%100))",
    "(l (v ?0 GEN%PVAR%0) (v ?0 GEN%PVAR%0))");
];;

register_tests "Term examples" (fun (x,y) -> assert_equal x y) examples;;
