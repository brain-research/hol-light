(*
This file contains proofs generatd by the neural theorem prover DeepHOL as proof
logs, and then converted to OCaml using DeepHOL's proof checker. We extended
HOL Light with the ability to swap out human written proofs by the synthetic
proofs listed in this file during its normal loading sequence. This process
requires the file import_proofs.ml to be loaded before, and
import_proofs_summary.ml to be loaded after HOL Light's loading sequence.
*)

set_jrh_lexer;;
open Lib;;
open Printer;;
open Theorem_fingerprint;;
open Import_proofs;;
open Tactics;;

Printer.current_encoding := Printer.Sexp;;


(* "|- !x. x = x <=> T" *)

register_proof 2109425092022597721 (
  fun () ->
    decode_goal [] "(a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun A (fun A (bool))) =) (v A x)) (v A x))) (c (bool) T))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !t1 t2. (\x. t1) t2 = t1" *)

register_proof 4452655282978696455 (
  fun () ->
    decode_goal [] "(a (c (fun (fun A (bool)) (bool)) !) (l (v A t1) (a (c (fun (fun B (bool)) (bool)) !) (l (v B t2) (a (a (c (fun A (fun A (bool))) =) (a (l (v B x) (v A t1)) (v B t2))) (v A t1))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !t1 t2. t1 \/ t2 <=> t2 \/ t1" *)

register_proof 986694786535376657 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (bool) (bool)) (bool)) !) (l (v (bool) t1) (a (c (fun (fun (bool) (bool)) (bool)) !) (l (v (bool) t2) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (v (bool) t1)) (v (bool) t2))) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (v (bool) t2)) (v (bool) t1)))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !p q r. (p \/ q) /\ r <=> p /\ r \/ q /\ r" *)

register_proof 1355080407099472983 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (bool) (bool)) (bool)) !) (l (v (bool) p) (a (c (fun (fun (bool) (bool)) (bool)) !) (l (v (bool) q) (a (c (fun (fun (bool) (bool)) (bool)) !) (l (v (bool) r) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (v (bool) p)) (v (bool) q))) (v (bool) r))) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (v (bool) p)) (v (bool) r))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (v (bool) q)) (v (bool) r))))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- (p <=> p') ==> (p' ==> (q <=> q')) ==> (p ==> q <=> p' ==> q')" *)

register_proof 600679008628789930 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) =) (v (bool) p)) (v (bool) p'))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) p')) (a (a (c (fun (bool) (fun (bool) (bool))) =) (v (bool) q)) (v (bool) q')))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) p)) (v (bool) q))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) p')) (v (bool) q')))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !P a. (?x. x = a /\ P x) <=> P a" *)

register_proof 3953023346715430631 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) P) (a (c (fun (fun A (bool)) (bool)) !) (l (v A a) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun A (bool)) (bool)) ?) (l (v A x) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun A (fun A (bool))) =) (v A x)) (v A a))) (a (v (fun A (bool)) P) (v A x)))))) (a (v (fun A (bool)) P) (v A a)))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !P Q. (!x. P x /\ Q x) <=> (!x. P x) /\ (!x. Q x)" *)

register_proof 1598846797968203432 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) P) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) Q) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (v (fun A (bool)) P) (v A x))) (a (v (fun A (bool)) Q) (v A x)))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (v (fun A (bool)) P) (v A x))))) (a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (v (fun A (bool)) Q) (v A x))))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !P Q. (?x. P x) \/ (?x. Q x) <=> (?x. P x \/ Q x)" *)

register_proof 2127739380704901188 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) P) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) Q) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (c (fun (fun A (bool)) (bool)) ?) (l (v A x) (a (v (fun A (bool)) P) (v A x))))) (a (c (fun (fun A (bool)) (bool)) ?) (l (v A x) (a (v (fun A (bool)) Q) (v A x)))))) (a (c (fun (fun A (bool)) (bool)) ?) (l (v A x) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (v (fun A (bool)) P) (v A x))) (a (v (fun A (bool)) Q) (v A x))))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !P Q. (?x. P /\ Q) <=> (?x. P) /\ (?x. Q)" *)

register_proof 1996195018145846150 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (bool) (bool)) (bool)) !) (l (v (bool) P) (a (c (fun (fun (bool) (bool)) (bool)) !) (l (v (bool) Q) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun A (bool)) (bool)) ?) (l (v A x) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (v (bool) P)) (v (bool) Q))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (c (fun (fun A (bool)) (bool)) ?) (l (v A x) (v (bool) P)))) (a (c (fun (fun A (bool)) (bool)) ?) (l (v A x) (v (bool) Q)))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !P Q. (!x. P) \/ (!x. Q) <=> (!x. P \/ Q)" *)

register_proof 1247328780493585448 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (bool) (bool)) (bool)) !) (l (v (bool) P) (a (c (fun (fun (bool) (bool)) (bool)) !) (l (v (bool) Q) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (v (bool) P)))) (a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (v (bool) Q))))) (a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (v (bool) P)) (v (bool) Q)))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !P Q. (!x. P ==> Q) <=> (?x. P) ==> (!x. Q)" *)

register_proof 4558772580413121024 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (bool) (bool)) (bool)) !) (l (v (bool) P) (a (c (fun (fun (bool) (bool)) (bool)) !) (l (v (bool) Q) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) P)) (v (bool) Q))))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (c (fun (fun A (bool)) (bool)) ?) (l (v A x) (v (bool) P)))) (a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (v (bool) Q)))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- (B ==> A) ==> ~A ==> ~B" *)

register_proof 748714918528269670 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) B)) (v (bool) A))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (c (fun (bool) (bool)) ~) (v (bool) A))) (a (c (fun (bool) (bool)) ~) (v (bool) B))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- (a <=> b) <=> (a ==> b) /\ (b ==> a)" *)

register_proof 3158068480668451995 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (bool) (fun (bool) (bool))) =) (v (bool) a)) (v (bool) b))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) a)) (v (bool) b))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) b)) (v (bool) a))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !x. (@y. y = x) = x" *)

register_proof 1665133142204816870 (
  fun () ->
    decode_goal [] "(a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (a (c (fun A (fun A (bool))) =) (a (c (fun (fun A (bool)) A) @) (l (v A y) (a (a (c (fun A (fun A (bool))) =) (v A y)) (v A x))))) (v A x))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !t. (t <=> T) \/ (t <=> F)" *)

register_proof 3940536481208086414 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (bool) (bool)) (bool)) !) (l (v (bool) t) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (a (c (fun (bool) (fun (bool) (bool))) =) (v (bool) t)) (c (bool) T))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (v (bool) t)) (c (bool) F)))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- (~P ==> F) ==> P" *)

register_proof 3909909050728755763 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (c (fun (bool) (bool)) ~) (v (bool) P))) (c (bool) F))) (v (bool) P))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !P. (?x. ~P x) <=> ~(!x. P x)" *)

register_proof 1403702005613348753 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) P) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun A (bool)) (bool)) ?) (l (v A x) (a (c (fun (bool) (bool)) ~) (a (v (fun A (bool)) P) (v A x)))))) (a (c (fun (bool) (bool)) ~) (a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (v (fun A (bool)) P) (v A x))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- (?b. P b) <=> P T \/ P F" *)

register_proof 1451827865767298714 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun (bool) (bool)) (bool)) ?) (l (v (bool) b) (a (v (fun (bool) (bool)) P) (v (bool) b))))) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (v (fun (bool) (bool)) P) (c (bool) T))) (a (v (fun (bool) (bool)) P) (c (bool) F))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !P Q. (!x. P x) \/ Q <=> (!x. P x \/ Q)" *)

register_proof 699976075396619947 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) P) (a (c (fun (fun (bool) (bool)) (bool)) !) (l (v (bool) Q) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (v (fun A (bool)) P) (v A x))))) (v (bool) Q))) (a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (v (fun A (bool)) P) (v A x))) (v (bool) Q)))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !b t. (if b then t else t) = t" *)

register_proof 2956146801143607915 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (bool) (bool)) (bool)) !) (l (v (bool) b) (a (c (fun (fun A (bool)) (bool)) !) (l (v A t) (a (a (c (fun A (fun A (bool))) =) (a (a (a (c (fun (bool) (fun A (fun A A))) COND) (v (bool) b)) (v A t)) (v A t))) (v A t))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- (A ==> B) /\ (C ==> D) ==> (if b then A else C) ==> (if b then B else D)" *)

register_proof 3217217488917139619 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) A)) (v (bool) B))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) C)) (v (bool) D)))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (a (c (fun (bool) (fun (bool) (fun (bool) (bool)))) COND) (v (bool) b)) (v (bool) A)) (v (bool) C))) (a (a (a (c (fun (bool) (fun (bool) (fun (bool) (bool)))) COND) (v (bool) b)) (v (bool) B)) (v (bool) D))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !P. (!x. ?!y. P x y) <=> (?f. !x y. P x y <=> f x = y)" *)

register_proof 1004008168637011311 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A (fun B (bool))) (bool)) (bool)) !) (l (v (fun A (fun B (bool))) P) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (c (fun (fun B (bool)) (bool)) ?!) (l (v B y) (a (a (v (fun A (fun B (bool))) P) (v A x)) (v B y))))))) (a (c (fun (fun (fun A B) (bool)) (bool)) ?) (l (v (fun A B) f) (a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (c (fun (fun B (bool)) (bool)) !) (l (v B y) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (v (fun A (fun B (bool))) P) (v A x)) (v B y))) (a (a (c (fun B (fun B (bool))) =) (a (v (fun A B) f) (v A x))) (v B y))))))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- ?b. b" *)

register_proof 2374932677073467653 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (bool) (bool)) (bool)) ?) (l (v (bool) b) (v (bool) b)))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- (p <=> q) <=> p /\ q \/ ~p /\ ~q" *)

register_proof 2076440368235384317 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (bool) (fun (bool) (bool))) =) (v (bool) p)) (v (bool) q))) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (v (bool) p)) (v (bool) q))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (c (fun (bool) (bool)) ~) (v (bool) p))) (a (c (fun (bool) (bool)) ~) (v (bool) q)))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- a /\ b ==> c <=> ~a \/ ~b \/ c" *)

register_proof 1953060264082078421 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (v (bool) a)) (v (bool) b))) (v (bool) c))) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (c (fun (bool) (bool)) ~) (v (bool) a))) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (c (fun (bool) (bool)) ~) (v (bool) b))) (v (bool) c))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- a /\ b \/ c <=> (a \/ c) /\ (b \/ c)" *)

register_proof 2271305540707422190 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (v (bool) a)) (v (bool) b))) (v (bool) c))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (v (bool) a)) (v (bool) c))) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (v (bool) b)) (v (bool) c))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- ((b <=> F) ==> (x <=> x0)) /\ ((b <=> T) ==> (x <=> x1))  ==> (x <=> (~b \/ x1) /\ (b \/ x0))" *)

register_proof 1808481953916382263 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) =) (v (bool) b)) (c (bool) F))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (v (bool) x)) (v (bool) x0)))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) =) (v (bool) b)) (c (bool) T))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (v (bool) x)) (v (bool) x1))))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (v (bool) x)) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (c (fun (bool) (bool)) ~) (v (bool) b))) (v (bool) x1))) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (v (bool) b)) (v (bool) x0)))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- ~a \/ ~b <=> ~(a /\ b)" *)

register_proof 2179085697761689359 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (c (fun (bool) (bool)) ~) (v (bool) a))) (a (c (fun (bool) (bool)) ~) (v (bool) b)))) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (v (bool) a)) (v (bool) b))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- x = x /\ (~(x = y) \/ ~(x = z) \/ y = z)" *)

register_proof 2909473506771129933 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun A (fun A (bool))) =) (v A x)) (v A x))) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (c (fun (bool) (bool)) ~) (a (a (c (fun A (fun A (bool))) =) (v A x)) (v A y)))) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (c (fun (bool) (bool)) ~) (a (a (c (fun A (fun A (bool))) =) (v A x)) (v A z)))) (a (a (c (fun A (fun A (bool))) =) (v A y)) (v A z)))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !a. a ==> ~a ==> F" *)

register_proof 3634915235263196553 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (bool) (bool)) (bool)) !) (l (v (bool) a) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) a)) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (c (fun (bool) (bool)) ~) (v (bool) a))) (c (bool) F)))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !a b. a ==> b <=> ~a \/ b" *)

register_proof 96506845967749275 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (bool) (bool)) (bool)) !) (l (v (bool) a) (a (c (fun (fun (bool) (bool)) (bool)) !) (l (v (bool) b) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) a)) (v (bool) b))) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (c (fun (bool) (bool)) ~) (v (bool) a))) (v (bool) b)))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- x ==> y /\ z <=> (x ==> y) /\ (x ==> z)" *)

register_proof 2378600398311076206 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) x)) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (v (bool) y)) (v (bool) z)))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) x)) (v (bool) y))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) x)) (v (bool) z))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- ((B ==> A) ==> D) ==> (A <=> B) ==> (A ==> B) /\ D" *)

register_proof 1305246386723748533 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) B)) (v (bool) A))) (v (bool) D))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) =) (v (bool) A)) (v (bool) B))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) A)) (v (bool) B))) (v (bool) D))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- (A ==> B) /\ (A ==> B ==> D ==> C) ==> (B ==> D) ==> A ==> C" *)

register_proof 2221066039138750292 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) A)) (v (bool) B))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) A)) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) B)) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) D)) (v (bool) C)))))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) B)) (v (bool) D))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) A)) (v (bool) C))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- (B ==> A) /\ (B ==> D ==> C) ==> B /\ D ==> A /\ C" *)

register_proof 405200044894797813 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) B)) (v (bool) A))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) B)) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) D)) (v (bool) C))))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (v (bool) B)) (v (bool) D))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (v (bool) A)) (v (bool) C))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- (P ==> (Q <=> R)) ==> (Q <=> (P ==> R) /\ (~P ==> Q))" *)

register_proof 4584785976104007141 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) P)) (a (a (c (fun (bool) (fun (bool) (bool))) =) (v (bool) Q)) (v (bool) R)))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (v (bool) Q)) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) P)) (v (bool) R))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (c (fun (bool) (bool)) ~) (v (bool) P))) (v (bool) Q)))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !f. (\(x,y). f x y) = (\p. f (FST p) (SND p))" *)

register_proof 4325565282365194851 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A (fun B C)) (bool)) (bool)) !) (l (v (fun A (fun B C)) f) (a (a (c (fun (fun (prod A B) C) (fun (fun (prod A B) C) (bool))) =) (a (c (fun (fun (fun (prod A B) C) (bool)) (fun (prod A B) C)) GABS) (l (v (fun (prod A B) C) f) (a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (c (fun (fun B (bool)) (bool)) !) (l (v B y) (a (a (c (fun C (fun C (bool))) GEQ) (a (v (fun (prod A B) C) f) (a (a (c (fun A (fun B (prod A B))) ,) (v A x)) (v B y)))) (a (a (v (fun A (fun B C)) f) (v A x)) (v B y)))))))))) (l (v (prod A B) p) (a (a (v (fun A (fun B C)) f) (a (c (fun (prod A B) A) FST) (v (prod A B) p))) (a (c (fun (prod A B) B) SND) (v (prod A B) p)))))))",
    Parse_tactic.parse "MP_TAC THM 336313455106353119" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 2315034643559247104 ; THM 4470124965642781870 ]") true;;

(* "|- !P. (@(x,y). P x y) = (@p. P (FST p) (SND p))" *)

register_proof 344703271449562383 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A (fun B (bool))) (bool)) (bool)) !) (l (v (fun A (fun B (bool))) P) (a (a (c (fun (prod A B) (fun (prod A B) (bool))) =) (a (c (fun (fun (prod A B) (bool)) (prod A B)) @) (a (c (fun (fun (fun (prod A B) (bool)) (bool)) (fun (prod A B) (bool))) GABS) (l (v (fun (prod A B) (bool)) f) (a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (c (fun (fun B (bool)) (bool)) !) (l (v B y) (a (a (c (fun (bool) (fun (bool) (bool))) GEQ) (a (v (fun (prod A B) (bool)) f) (a (a (c (fun A (fun B (prod A B))) ,) (v A x)) (v B y)))) (a (a (v (fun A (fun B (bool))) P) (v A x)) (v B y))))))))))) (a (c (fun (fun (prod A B) (bool)) (prod A B)) @) (l (v (prod A B) p) (a (a (v (fun A (fun B (bool))) P) (a (c (fun (prod A B) A) FST) (v (prod A B) p))) (a (c (fun (prod A B) B) SND) (v (prod A B) p))))))))",
    Parse_tactic.parse "MP_TAC THM 4592450197497463396" THEN
    Parse_tactic.parse "ONCE_REWRITE_TAC [ ]" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 4325565282365194851 ]") true;;

(* "|- !PRG PRG'.    (!a0 a1. PRG a0 a1 ==> PRG' a0 a1)    ==> (!a0 a1.         a0 = _0 /\ a1 = e \/         (?b n. a0 = SUC n /\ a1 = f b n /\ PRG n b)         ==> a0 = _0 /\ a1 = e \/           (?b n. a0 = SUC n /\ a1 = f b n /\ PRG' n b))" *)

register_proof 2535797612939048549 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun (num) (fun A (bool))) (bool)) (bool)) !) (l (v (fun (num) (fun A (bool))) PRG) (a (c (fun (fun (fun (num) (fun A (bool))) (bool)) (bool)) !) (l (v (fun (num) (fun A (bool))) PRG') (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) a0) (a (c (fun (fun A (bool)) (bool)) !) (l (v A a1) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (v (fun (num) (fun A (bool))) PRG) (v (num) a0)) (v A a1))) (a (a (v (fun (num) (fun A (bool))) PRG') (v (num) a0)) (v A a1)))))))) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) a0) (a (c (fun (fun A (bool)) (bool)) !) (l (v A a1) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) a0)) (c (num) _0))) (a (a (c (fun A (fun A (bool))) =) (v A a1)) (v A e)))) (a (c (fun (fun A (bool)) (bool)) ?) (l (v A b) (a (c (fun (fun (num) (bool)) (bool)) ?) (l (v (num) n) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) a0)) (a (c (fun (num) (num)) SUC) (v (num) n)))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun A (fun A (bool))) =) (v A a1)) (a (a (v (fun A (fun (num) A)) f) (v A b)) (v (num) n)))) (a (a (v (fun (num) (fun A (bool))) PRG) (v (num) n)) (v A b)))))))))) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) a0)) (c (num) _0))) (a (a (c (fun A (fun A (bool))) =) (v A a1)) (v A e)))) (a (c (fun (fun A (bool)) (bool)) ?) (l (v A b) (a (c (fun (fun (num) (bool)) (bool)) ?) (l (v (num) n) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) a0)) (a (c (fun (num) (num)) SUC) (v (num) n)))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun A (fun A (bool))) =) (v A a1)) (a (a (v (fun A (fun (num) A)) f) (v A b)) (v (num) n)))) (a (a (v (fun (num) (fun A (bool))) PRG') (v (num) n)) (v A b)))))))))))))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !m. m + 0 = m" *)

register_proof 3184848161821383788 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) m)) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (num) m))))",
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- m + n = n + m /\ (m + n) + p = m + n + p /\ m + n + p = n + m + p" *)

register_proof 2655919829942423315 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) m)) (v (num) n))) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) n)) (v (num) m)))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) +) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) m)) (v (num) n))) (v (num) p))) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) m)) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) n)) (v (num) p))))) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) m)) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) n)) (v (num) p)))) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) n)) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) m)) (v (num) p))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1300629121302339976 ; THM 4383546379451555841 ]") true;;

(* "|- !m n. m + n = n <=> m = 0" *)

register_proof 1841765690811148654 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) m)) (v (num) n))) (v (num) n))) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) m)) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))))))",
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- 1 = SUC 0" *)

register_proof 2039837508928286649 (
  fun () ->
    decode_goal [] "(a (a (c (fun (num) (fun (num) (bool))) =) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT1) (c (num) _0)))) (a (c (fun (num) (num)) SUC) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 4311407572609110021 ; THM 77677741766225983 ; THM 3695109707437612667 ]") true;;

(* "|- (!n. 0 * n = 0) /\  (!m. m * 0 = 0) /\  (!n. 1 * n = n) /\  (!m. m * 1 = m) /\  (!m n. SUC m * n = m * n + n) /\  (!m n. m * SUC n = m + m * n)" *)

register_proof 2785399557515712619 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) *) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))) (v (num) n))) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) m)) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) *) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT1) (c (num) _0)))) (v (num) n))) (v (num) n))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) m)) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT1) (c (num) _0))))) (v (num) m))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) *) (a (c (fun (num) (num)) SUC) (v (num) m))) (v (num) n))) (a (a (c (fun (num) (fun (num) (num))) +) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) m)) (v (num) n))) (v (num) n)))))))) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) m)) (a (c (fun (num) (num)) SUC) (v (num) n)))) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) m)) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) m)) (v (num) n)))))))))))))",
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- m * n = n * m /\ (m * n) * p = m * n * p /\ m * n * p = n * m * p" *)

register_proof 3860008140353490501 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) m)) (v (num) n))) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) n)) (v (num) m)))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) *) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) m)) (v (num) n))) (v (num) p))) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) m)) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) n)) (v (num) p))))) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) m)) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) n)) (v (num) p)))) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) n)) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) m)) (v (num) p))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3066286267198787148 ; THM 716869982337045710 ]") true;;

(* "|- !n. n EXP 1 = n" *)

register_proof 2045356151356462258 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) EXP) (v (num) n)) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT1) (c (num) _0))))) (v (num) n))))",
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- !m n. m < SUC n <=> m <= n" *)

register_proof 1310608256656569969 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (num) (fun (num) (bool))) <) (v (num) m)) (a (c (fun (num) (num)) SUC) (v (num) n)))) (a (a (c (fun (num) (fun (num) (bool))) <=) (v (num) m)) (v (num) n)))))))",
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- !n. n <= n" *)

register_proof 2012746649633913608 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (num) (fun (num) (bool))) <=) (v (num) n)) (v (num) n))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1310608256656569969 ; THM 2137115981466194412 ]") true;;

(* "|- !m n. ~(m < n /\ n <= m)" *)

register_proof 550346460137206759 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (num) (fun (num) (bool))) <) (v (num) m)) (v (num) n))) (a (a (c (fun (num) (fun (num) (bool))) <=) (v (num) n)) (v (num) m))))))))",
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- !m n. m <= n \/ n <= m" *)

register_proof 130140084316368533 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (a (c (fun (num) (fun (num) (bool))) <=) (v (num) m)) (v (num) n))) (a (a (c (fun (num) (fun (num) (bool))) <=) (v (num) n)) (v (num) m)))))))",
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- !m n. m < n <=> m <= n /\ ~(m = n)" *)

register_proof 1074197768577037275 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (num) (fun (num) (bool))) <) (v (num) m)) (v (num) n))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (num) (fun (num) (bool))) <=) (v (num) m)) (v (num) n))) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) m)) (v (num) n)))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3726646019250111179 ; THM 1363642897514818795 ]") true;;

(* "|- !m n. m < n <=> (?d. n = m + SUC d)" *)

register_proof 3804232667303855119 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (num) (fun (num) (bool))) <) (v (num) m)) (v (num) n))) (a (c (fun (fun (num) (bool)) (bool)) ?) (l (v (num) d) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) n)) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) m)) (a (c (fun (num) (num)) SUC) (v (num) d)))))))))))",
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 3498817178684150623 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 526993218849719811 ; THM 3498817178684150623 ; THM 1218939027586461897 ]") true;;

(* "|- !m n p. m + n <= m + p <=> n <= p" *)

register_proof 2295601898897976418 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) p) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (num) (fun (num) (bool))) <=) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) m)) (v (num) n))) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) m)) (v (num) p)))) (a (a (c (fun (num) (fun (num) (bool))) <=) (v (num) n)) (v (num) p)))))))))",
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- !m n p q. m <= p /\ n < q ==> m + n < p + q" *)

register_proof 464178018037037789 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) p) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) q) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (num) (fun (num) (bool))) <=) (v (num) m)) (v (num) p))) (a (a (c (fun (num) (fun (num) (bool))) <) (v (num) n)) (v (num) q)))) (a (a (c (fun (num) (fun (num) (bool))) <) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) m)) (v (num) n))) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) p)) (v (num) q))))))))))))",
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- !n. ~(EVEN n /\ ODD n)" *)

register_proof 4160910177796689045 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (c (fun (num) (bool)) EVEN) (v (num) n))) (a (c (fun (num) (bool)) ODD) (v (num) n))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 2848599169349786691 ]") true;;

(* "|- EVEN m \/ EVEN m /\ ~(n = 0) <=> EVEN m" *)

register_proof 920209004642439711 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (c (fun (num) (bool)) EVEN) (v (num) m))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (c (fun (num) (bool)) EVEN) (v (num) m))) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) n)) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))))) (a (c (fun (num) (bool)) EVEN) (v (num) m)))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !m n. ODD (m * n) <=> ODD m /\ ODD n" *)

register_proof 2873115296042991567 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (num) (bool)) ODD) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) m)) (v (num) n)))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (c (fun (num) (bool)) ODD) (v (num) m))) (a (c (fun (num) (bool)) ODD) (v (num) n))))))))",
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 1519551676238234042 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 746166925954020569 ; THM 677849830807999624 ]") true;;

(* "|- !n. EVEN (2 * n)" *)

register_proof 62096996627259201 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (c (fun (num) (bool)) EVEN) (a (a (c (fun (num) (fun (num) (num))) *) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT0) (a (c (fun (num) (num)) BIT1) (c (num) _0))))) (v (num) n)))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 1374621388813277359 ; THM 746166925954020569 ; THM 517432437841706505 ; THM 1431862801918340860 ]") true;;

(* "|- !m n. (m + n) - n = m" *)

register_proof 1339857128145214262 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) -) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) m)) (v (num) n))) (v (num) n))) (v (num) m))))))",
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- !m n. n <= m ==> m - n + n = m" *)

register_proof 2076661252179783845 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (num) (fun (num) (bool))) <=) (v (num) n)) (v (num) m))) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) +) (a (a (c (fun (num) (fun (num) (num))) -) (v (num) m)) (v (num) n))) (v (num) n))) (v (num) m)))))))",
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- !n. SUC n - 1 = n" *)

register_proof 1358121022213652737 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) -) (a (c (fun (num) (num)) SUC) (v (num) n))) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT1) (c (num) _0))))) (v (num) n))))",
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- !m n. ~(n = 0) ==> m = m DIV n * n + m MOD n /\ m MOD n < n" *)

register_proof 3928255843600760221 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) n)) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) m)) (a (a (c (fun (num) (fun (num) (num))) +) (a (a (c (fun (num) (fun (num) (num))) *) (a (a (c (fun (num) (fun (num) (num))) DIV) (v (num) m)) (v (num) n))) (v (num) n))) (a (a (c (fun (num) (fun (num) (num))) MOD) (v (num) m)) (v (num) n))))) (a (a (c (fun (num) (fun (num) (bool))) <) (a (a (c (fun (num) (fun (num) (num))) MOD) (v (num) m)) (v (num) n))) (v (num) n))))))))",
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- !m n q r. m = q * n + r /\ r < n ==> m DIV n = q" *)

register_proof 4488956382140193966 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) q) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) r) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) m)) (a (a (c (fun (num) (fun (num) (num))) +) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) q)) (v (num) n))) (v (num) r)))) (a (a (c (fun (num) (fun (num) (bool))) <) (v (num) r)) (v (num) n)))) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) DIV) (v (num) m)) (v (num) n))) (v (num) q)))))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1333515036398999306 ]") true;;

(* "|- !a b n. ~(a = 0) /\ b <= a * n ==> b DIV a <= n" *)

register_proof 2045224807044483958 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) a) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) b) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) a)) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))) (a (a (c (fun (num) (fun (num) (bool))) <=) (v (num) b)) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) a)) (v (num) n))))) (a (a (c (fun (num) (fun (num) (bool))) <=) (a (a (c (fun (num) (fun (num) (num))) DIV) (v (num) b)) (v (num) a))) (v (num) n)))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1037808187268192550 ; THM 4095241785075778229 ; THM 3608212069811132254 ]") true;;

(* "|- !m n p. ~(p = 0) /\ p <= m ==> n DIV m <= n DIV p" *)

register_proof 2616035583359667375 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) p) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) p)) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))) (a (a (c (fun (num) (fun (num) (bool))) <=) (v (num) p)) (v (num) m)))) (a (a (c (fun (num) (fun (num) (bool))) <=) (a (a (c (fun (num) (fun (num) (num))) DIV) (v (num) n)) (v (num) m))) (a (a (c (fun (num) (fun (num) (num))) DIV) (v (num) n)) (v (num) p))))))))))",
    Parse_tactic.parse "MP_TAC THM 2621068103100749493" THEN
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 1041278397167009944 ]" THEN
    Parse_tactic.parse "MP_TAC THM 1124271433710135282" THEN
    Parse_tactic.parse "SIMP_TAC [ THM 1041278397167009944 ; THM 4432927027650125967 ; THM 2621068103100749493 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 4095241785075778229 ; THM 2653348955385406708 ; THM 3608212069811132254 ; THM 406147607937521369 ]") true;;

(* "|- !R. (!x. R x x) /\ (!x y z. R x y /\ R y z ==> R x z)    ==> ((!m n. m <= n ==> R m n) <=> (!n. R n (SUC n)))" *)

register_proof 3123609648886345195 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun (num) (fun (num) (bool))) (bool)) (bool)) !) (l (v (fun (num) (fun (num) (bool))) R) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) x) (a (a (v (fun (num) (fun (num) (bool))) R) (v (num) x)) (v (num) x))))) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) x) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) y) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) z) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (v (fun (num) (fun (num) (bool))) R) (v (num) x)) (v (num) y))) (a (a (v (fun (num) (fun (num) (bool))) R) (v (num) y)) (v (num) z)))) (a (a (v (fun (num) (fun (num) (bool))) R) (v (num) x)) (v (num) z))))))))))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (num) (fun (num) (bool))) <=) (v (num) m)) (v (num) n))) (a (a (v (fun (num) (fun (num) (bool))) R) (v (num) m)) (v (num) n)))))))) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (v (fun (num) (fun (num) (bool))) R) (v (num) n)) (a (c (fun (num) (num)) SUC) (v (num) n)))))))))",
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 1953060264082078421 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 526993218849719811 ; THM 3961773322428078288 ; THM 3288246834660693769 ]") true;;

(* "|- WF (<)" *)

register_proof 4364558322235824273 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (fun (num) (bool))) (bool)) WF) (c (fun (num) (fun (num) (bool))) <))",
    Parse_tactic.parse "MP_TAC THM 1363642897514818795" THEN
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 4395984219505846894 ; THM 1363642897514818795 ]" THEN
    Parse_tactic.parse "REWRITE_TAC [ ]" THEN
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 26066671613309777 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !x. WF (<<) ==> ~(x << x)" *)

register_proof 1068240823160301190 (
  fun () ->
    decode_goal [] "(a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (c (fun (fun A (fun A (bool))) (bool)) WF) (v (fun A (fun A (bool))) <<))) (a (c (fun (bool) (bool)) ~) (a (a (v (fun A (fun A (bool))) <<) (v A x)) (v A x))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 4395984219505846894 ]") true;;

(* "|- (!m. P m ==> m = e ==> Q) <=> P e ==> Q" *)

register_proof 3920459092661566846 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun ?14485 (bool)) (bool)) !) (l (v ?14485 m) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (v (fun ?14485 (bool)) P) (v ?14485 m))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun ?14485 (fun ?14485 (bool))) =) (v ?14485 m)) (v ?14485 e))) (v (bool) Q)))))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (v (fun ?14485 (bool)) P) (v ?14485 e))) (v (bool) Q)))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- n = a + p * b <=> BIT0 n = BIT0 a + BIT0 p * b" *)

register_proof 2273670329058414971 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) n)) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) a)) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) p)) (v (num) b))))) (a (a (c (fun (num) (fun (num) (bool))) =) (a (c (fun (num) (num)) BIT0) (v (num) n))) (a (a (c (fun (num) (fun (num) (num))) +) (a (c (fun (num) (num)) BIT0) (v (num) a))) (a (a (c (fun (num) (fun (num) (num))) *) (a (c (fun (num) (num)) BIT0) (v (num) p))) (v (num) b)))))",
    Parse_tactic.parse "MP_TAC THM 1431862801918340860" THEN
    Parse_tactic.parse "SIMP_TAC [ ]" THEN
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- BIT0 a + BIT0 p * b = BIT0 (a + p * b)" *)

register_proof 602095365235289027 (
  fun () ->
    decode_goal [] "(a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) +) (a (c (fun (num) (num)) BIT0) (v (num) a))) (a (a (c (fun (num) (fun (num) (num))) *) (a (c (fun (num) (num)) BIT0) (v (num) p))) (v (num) b)))) (a (c (fun (num) (num)) BIT0) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) a)) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) p)) (v (num) b)))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3267015526949336826 ; THM 3605020100906670618 ]") true;;

(* "|- _0 EXP 2 = _0" *)

register_proof 3220200960775105387 (
  fun () ->
    decode_goal [] "(a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) EXP) (c (num) _0)) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT0) (a (c (fun (num) (num)) BIT1) (c (num) _0)))))) (c (num) _0))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 339257798805081858 ; THM 282981955280276782 ]") true;;

(* "|- (m * n = p <=> BIT0 m * n = BIT0 p) /\ (m * n = p <=> m * BIT0 n = BIT0 p)" *)

register_proof 3161161789321011845 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) m)) (v (num) n))) (v (num) p))) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) *) (a (c (fun (num) (num)) BIT0) (v (num) m))) (v (num) n))) (a (c (fun (num) (num)) BIT0) (v (num) p))))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) m)) (v (num) n))) (v (num) p))) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) *) (v (num) m)) (a (c (fun (num) (num)) BIT0) (v (num) n)))) (a (c (fun (num) (num)) BIT0) (v (num) p)))))",
    Parse_tactic.parse "MP_TAC THM 1431862801918340860" THEN
    Parse_tactic.parse "SIMP_TAC [ ]" THEN
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- m + n = p <=> NUMERAL m + NUMERAL n = NUMERAL p" *)

register_proof 2408496541630043358 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) m)) (v (num) n))) (v (num) p))) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) +) (a (c (fun (num) (num)) NUMERAL) (v (num) m))) (a (c (fun (num) (num)) NUMERAL) (v (num) n)))) (a (c (fun (num) (num)) NUMERAL) (v (num) p))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1374621388813277359 ]") true;;

(* "|- m EXP _0 = BIT1 _0" *)

register_proof 3470230647555053391 (
  fun () ->
    decode_goal [] "(a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) EXP) (v (num) m)) (c (num) _0))) (a (c (fun (num) (num)) BIT1) (c (num) _0)))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1166015664794626049 ; THM 1374621388813277359 ]") true;;

(* "|- NUMERAL n < NUMERAL n <=> F" *)

register_proof 1931650813387793311 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (num) (fun (num) (bool))) <) (a (c (fun (num) (num)) NUMERAL) (v (num) n))) (a (c (fun (num) (num)) NUMERAL) (v (num) n)))) (c (bool) F))",
    Parse_tactic.parse "REWRITE_TAC [ THM 1363642897514818795 ]") true;;

(* "|- SUC (m + p) = n ==> (NUMERAL n = NUMERAL p <=> F)" *)

register_proof 234385277368927905 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (num) (fun (num) (bool))) =) (a (c (fun (num) (num)) SUC) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) m)) (v (num) p)))) (v (num) n))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (num) (fun (num) (bool))) =) (a (c (fun (num) (num)) NUMERAL) (v (num) n))) (a (c (fun (num) (num)) NUMERAL) (v (num) p)))) (c (bool) F)))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 2539491598447268337 ]") true;;

(* "|- m + n = p ==> p - n = m" *)

register_proof 423314891495195721 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) m)) (v (num) n))) (v (num) p))) (a (a (c (fun (num) (fun (num) (bool))) =) (a (a (c (fun (num) (fun (num) (num))) -) (v (num) p)) (v (num) n))) (v (num) m)))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1339857128145214262 ]") true;;

(* "|- (!n. n < SUC k ==> P n) <=> (!n. n < k ==> P n) /\ P k" *)

register_proof 2370753161275747746 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (num) (fun (num) (bool))) <) (v (num) n)) (a (c (fun (num) (num)) SUC) (v (num) k)))) (a (v (fun (num) (bool)) P) (v (num) n)))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (num) (fun (num) (bool))) <) (v (num) n)) (v (num) k))) (a (v (fun (num) (bool)) P) (v (num) n)))))) (a (v (fun (num) (bool)) P) (v (num) k))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 2137115981466194412 ]") true;;

(* "|- (EVEN x <=> (!y. ~(x = SUC (2 * y)))) /\  (ODD x <=> (!y. ~(x = 2 * y))) /\  (~EVEN x <=> (!y. ~(x = 2 * y))) /\  (~ODD x <=> (!y. ~(x = SUC (2 * y))))" *)

register_proof 4012028389749247251 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (num) (bool)) EVEN) (v (num) x))) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) y) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) x)) (a (c (fun (num) (num)) SUC) (a (a (c (fun (num) (fun (num) (num))) *) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT0) (a (c (fun (num) (num)) BIT1) (c (num) _0))))) (v (num) y))))))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (num) (bool)) ODD) (v (num) x))) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) y) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) x)) (a (a (c (fun (num) (fun (num) (num))) *) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT0) (a (c (fun (num) (num)) BIT1) (c (num) _0))))) (v (num) y)))))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (bool) (bool)) ~) (a (c (fun (num) (bool)) EVEN) (v (num) x)))) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) y) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) x)) (a (a (c (fun (num) (fun (num) (num))) *) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT0) (a (c (fun (num) (num)) BIT1) (c (num) _0))))) (v (num) y)))))))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (bool) (bool)) ~) (a (c (fun (num) (bool)) ODD) (v (num) x)))) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) y) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) x)) (a (c (fun (num) (num)) SUC) (a (a (c (fun (num) (fun (num) (num))) *) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT0) (a (c (fun (num) (num)) BIT1) (c (num) _0))))) (v (num) y)))))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3381743554119077386 ; THM 677849830807999624 ; THM 296127638369908190 ]") true;;

(* "|- a ==> F <=> ~a" *)

register_proof 3382299821298012862 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) a)) (c (bool) F))) (a (c (fun (bool) (bool)) ~) (v (bool) a)))",
    Parse_tactic.parse "REWRITE_TAC [ ]") true;;

(* "|- (?) P ==> c = (@) P ==> P c" *)

register_proof 612496176720989710 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (c (fun (fun ?21558 (bool)) (bool)) ?) (v (fun ?21558 (bool)) P))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun ?21558 (fun ?21558 (bool))) =) (v ?21558 c)) (a (c (fun (fun ?21558 (bool)) ?21558) @) (v (fun ?21558 (bool)) P)))) (a (v (fun ?21558 (bool)) P) (v ?21558 c))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 421915263640211727 ]") true;;

(* "|- (!x. x = a /\ p x ==> q x) <=> p a ==> q a" *)

register_proof 4315766930732031189 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun ?21943 (bool)) (bool)) !) (l (v ?21943 x) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun ?21943 (fun ?21943 (bool))) =) (v ?21943 x)) (v ?21943 a))) (a (v (fun ?21943 (bool)) p) (v ?21943 x)))) (a (v (fun ?21943 (bool)) q) (v ?21943 x)))))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (v (fun ?21943 (bool)) p) (v ?21943 a))) (a (v (fun ?21943 (bool)) q) (v ?21943 a))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- ZIP [] [] = [] /\ ZIP (CONS h1 t1) (CONS h2 t2) = CONS (h1,h2) (ZIP t1 t2)" *)

register_proof 4357068969105579624 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (list (prod ?22648 ?22649)) (fun (list (prod ?22648 ?22649)) (bool))) =) (a (a (c (fun (list ?22648) (fun (list ?22649) (list (prod ?22648 ?22649)))) ZIP) (c (list ?22648) NIL)) (c (list ?22649) NIL))) (c (list (prod ?22648 ?22649)) NIL))) (a (a (c (fun (list (prod ?22673 ?22674)) (fun (list (prod ?22673 ?22674)) (bool))) =) (a (a (c (fun (list ?22673) (fun (list ?22674) (list (prod ?22673 ?22674)))) ZIP) (a (a (c (fun ?22673 (fun (list ?22673) (list ?22673))) CONS) (v ?22673 h1)) (v (list ?22673) t1))) (a (a (c (fun ?22674 (fun (list ?22674) (list ?22674))) CONS) (v ?22674 h2)) (v (list ?22674) t2)))) (a (a (c (fun (prod ?22673 ?22674) (fun (list (prod ?22673 ?22674)) (list (prod ?22673 ?22674)))) CONS) (a (a (c (fun ?22673 (fun ?22674 (prod ?22673 ?22674))) ,) (v ?22673 h1)) (v ?22674 h2))) (a (a (c (fun (list ?22673) (fun (list ?22674) (list (prod ?22673 ?22674)))) ZIP) (v (list ?22673) t1)) (v (list ?22674) t2)))))",
    Parse_tactic.parse "CONJ_TAC" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 2633484072293537902 ]" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 1606512998315491700 ; THM 2633484072293537902 ; THM 1913476659702935606 ]") true;;

(* "|- !l m. APPEND l m = [] <=> l = [] /\ m = []" *)

register_proof 3618448832348959736 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (list ?24272) (bool)) (bool)) !) (l (v (list ?24272) l) (a (c (fun (fun (list ?24272) (bool)) (bool)) !) (l (v (list ?24272) m) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (list ?24272) (fun (list ?24272) (bool))) =) (a (a (c (fun (list ?24272) (fun (list ?24272) (list ?24272))) APPEND) (v (list ?24272) l)) (v (list ?24272) m))) (c (list ?24272) NIL))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (list ?24272) (fun (list ?24272) (bool))) =) (v (list ?24272) l)) (c (list ?24272) NIL))) (a (a (c (fun (list ?24272) (fun (list ?24272) (bool))) =) (v (list ?24272) m)) (c (list ?24272) NIL))))))))",
    Parse_tactic.parse "MP_TAC THM 3012088353860710806" THEN
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 1953060264082078421 ]" THEN
    Parse_tactic.parse "MP_TAC THM 3012088353860710806" THEN
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 1953060264082078421 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3878772702663538657 ; THM 1307738859393824562 ; THM 634128687273288853 ]") true;;

(* "|- (if p then T else q) <=> ~p ==> q" *)

register_proof 4461332045104214760 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (a (c (fun (bool) (fun (bool) (fun (bool) (bool)))) COND) (v (bool) p)) (c (bool) T)) (v (bool) q))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (c (fun (bool) (bool)) ~) (v (bool) p))) (v (bool) q)))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !n. dist (n,0) = n" *)

register_proof 91282620995974401 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (num) (fun (num) (bool))) =) (a (c (fun (prod (num) (num)) (num)) dist) (a (a (c (fun (num) (fun (num) (prod (num) (num)))) ,) (v (num) n)) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))) (v (num) n))))",
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 1902057480359981979 ]" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 3865203227777516316 ; THM 1540997764389850600 ; THM 1374621388813277359 ; THM 4370731899580733716 ; THM 3288246834660693769 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3498817178684150623 ; THM 1374621388813277359 ]") true;;

(* "|- !m n. dist (m,m + n) = n" *)

register_proof 44424774430021872 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (num) (fun (num) (bool))) =) (a (c (fun (prod (num) (num)) (num)) dist) (a (a (c (fun (num) (fun (num) (prod (num) (num)))) ,) (v (num) m)) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) m)) (v (num) n))))) (v (num) n))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1540997764389850600 ; THM 2655919829942423315 ; THM 1102461771216822229 ]") true;;

(* "|- !m n p. dist (m,n) <= p <=> m <= n + p /\ n <= m + p" *)

register_proof 4317726714029075271 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) p) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (num) (fun (num) (bool))) <=) (a (c (fun (prod (num) (num)) (num)) dist) (a (a (c (fun (num) (fun (num) (prod (num) (num)))) ,) (v (num) m)) (v (num) n)))) (v (num) p))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (num) (fun (num) (bool))) <=) (v (num) m)) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) n)) (v (num) p)))) (a (a (c (fun (num) (fun (num) (bool))) <=) (v (num) n)) (a (a (c (fun (num) (fun (num) (num))) +) (v (num) m)) (v (num) p)))))))))))",
    Parse_tactic.parse "MP_TAC THM 1540997764389850600" THEN
    Parse_tactic.parse "SIMP_TAC [ ]" THEN
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- ~a ==> b <=> a \/ b" *)

register_proof 2904491177385481701 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (c (fun (bool) (bool)) ~) (v (bool) a))) (v (bool) b))) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (v (bool) a)) (v (bool) b)))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !m n. m = n ==> & m === & n" *)

register_proof 195271261186612111 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) m)) (v (num) n))) (a (a (c (fun (nadd) (fun (nadd) (bool))) nadd_eq) (a (c (fun (num) (nadd)) nadd_of_num) (v (num) m))) (a (c (fun (num) (nadd)) nadd_of_num) (v (num) n))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 2239113286456509746 ]") true;;

(* "|- !x y z. x <<= y /\ y <<= z ==> x <<= z" *)

register_proof 2975289850649652782 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (nadd) (bool)) (bool)) !) (l (v (nadd) x) (a (c (fun (fun (nadd) (bool)) (bool)) !) (l (v (nadd) y) (a (c (fun (fun (nadd) (bool)) (bool)) !) (l (v (nadd) z) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (nadd) (fun (nadd) (bool))) nadd_le) (v (nadd) x)) (v (nadd) y))) (a (a (c (fun (nadd) (fun (nadd) (bool))) nadd_le) (v (nadd) y)) (v (nadd) z)))) (a (a (c (fun (nadd) (fun (nadd) (bool))) nadd_le) (v (nadd) x)) (v (nadd) z)))))))))",
    Parse_tactic.parse "MP_TAC THM 3978200949876665525" THEN
    Parse_tactic.parse "SIMP_TAC [ ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 865575319150772161 ; THM 2655919829942423315 ; THM 3608212069811132254 ]") true;;

(* "|- !x y. x ++ y === y ++ x" *)

register_proof 280516418529106767 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (nadd) (bool)) (bool)) !) (l (v (nadd) x) (a (c (fun (fun (nadd) (bool)) (bool)) !) (l (v (nadd) y) (a (a (c (fun (nadd) (fun (nadd) (bool))) nadd_eq) (a (a (c (fun (nadd) (fun (nadd) (nadd))) nadd_add) (v (nadd) x)) (v (nadd) y))) (a (a (c (fun (nadd) (fun (nadd) (nadd))) nadd_add) (v (nadd) y)) (v (nadd) x)))))))",
    Parse_tactic.parse "MP_TAC THM 816429913600125137" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 4317726714029075271 ; THM 2655919829942423315 ; THM 526993218849719811 ; THM 816429913600125137 ; THM 2205080817962225807 ; THM 3739160786816999866 ; THM 1519551676238234042 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !x. & 1 ** x === x" *)

register_proof 1218001966535281995 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (nadd) (bool)) (bool)) !) (l (v (nadd) x) (a (a (c (fun (nadd) (fun (nadd) (bool))) nadd_eq) (a (a (c (fun (nadd) (fun (nadd) (nadd))) nadd_mul) (a (c (fun (num) (nadd)) nadd_of_num) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT1) (c (num) _0))))) (v (nadd) x))) (v (nadd) x))))",
    Parse_tactic.parse "MP_TAC THM 816429913600125137" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 4317726714029075271 ; THM 816429913600125137 ; THM 526993218849719811 ; THM 2655919829942423315 ; THM 1120429645677720577 ; THM 897710453187867535 ; THM 3739160786816999866 ; THM 2811381786271539603 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1637178899274243946 ]") true;;

(* "|- !x. & 0 <<= x" *)

register_proof 3695650588565175443 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (nadd) (bool)) (bool)) !) (l (v (nadd) x) (a (a (c (fun (nadd) (fun (nadd) (bool))) nadd_le) (a (c (fun (num) (nadd)) nadd_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (nadd) x))))",
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 625367087922838820 ]" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 3978200949876665525 ; THM 1120429645677720577 ; THM 1637178899274243946 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 4160059429278996035 ]") true;;

(* "|- !x y z. x ++ y <<= x ++ z <=> y <<= z" *)

register_proof 2941317540073966044 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (nadd) (bool)) (bool)) !) (l (v (nadd) x) (a (c (fun (fun (nadd) (bool)) (bool)) !) (l (v (nadd) y) (a (c (fun (fun (nadd) (bool)) (bool)) !) (l (v (nadd) z) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (nadd) (fun (nadd) (bool))) nadd_le) (a (a (c (fun (nadd) (fun (nadd) (nadd))) nadd_add) (v (nadd) x)) (v (nadd) y))) (a (a (c (fun (nadd) (fun (nadd) (nadd))) nadd_add) (v (nadd) x)) (v (nadd) z)))) (a (a (c (fun (nadd) (fun (nadd) (bool))) nadd_le) (v (nadd) y)) (v (nadd) z)))))))))",
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 4316752623609269416 ]" THEN
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 1519551676238234042 ]" THEN
    Parse_tactic.parse "MP_TAC THM 3978200949876665525" THEN
    Parse_tactic.parse "MP_TAC THM 3978200949876665525" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 3978200949876665525 ; THM 526993218849719811 ; THM 2655919829942423315 ; THM 2205080817962225807 ; THM 1519551676238234042 ]" THEN
    Parse_tactic.parse "MESON_TAC [ THM 2655919829942423315 ; THM 1120429645677720577 ; THM 3288246834660693769 ; THM 4515521160539924975 ]") true;;

(* "|- (a \/ b) /\ ~(c /\ b) ==> c ==> a" *)

register_proof 3146205164861832236 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (v (bool) a)) (v (bool) b))) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (v (bool) c)) (v (bool) b))))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) c)) (v (bool) a)))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !x y. x treal_mul y treal_eq y treal_mul x" *)

register_proof 2329894969025548711 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (prod (hreal) (hreal)) (bool)) (bool)) !) (l (v (prod (hreal) (hreal)) x) (a (c (fun (fun (prod (hreal) (hreal)) (bool)) (bool)) !) (l (v (prod (hreal) (hreal)) y) (a (a (c (fun (prod (hreal) (hreal)) (fun (prod (hreal) (hreal)) (bool))) treal_eq) (a (a (c (fun (prod (hreal) (hreal)) (fun (prod (hreal) (hreal)) (prod (hreal) (hreal)))) treal_mul) (v (prod (hreal) (hreal)) x)) (v (prod (hreal) (hreal)) y))) (a (a (c (fun (prod (hreal) (hreal)) (fun (prod (hreal) (hreal)) (prod (hreal) (hreal)))) treal_mul) (v (prod (hreal) (hreal)) y)) (v (prod (hreal) (hreal)) x)))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 2660370635215748600 ; THM 4159842767006411438 ]") true;;

(* "|- treal_inv (treal_of_num 0) treal_eq treal_of_num 0" *)

register_proof 2085659583793638054 (
  fun () ->
    decode_goal [] "(a (a (c (fun (prod (hreal) (hreal)) (fun (prod (hreal) (hreal)) (bool))) treal_eq) (a (c (fun (prod (hreal) (hreal)) (prod (hreal) (hreal))) treal_inv) (a (c (fun (num) (prod (hreal) (hreal))) treal_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))) (a (c (fun (num) (prod (hreal) (hreal))) treal_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 2660370635215748600 ; THM 4034021265367232662 ; THM 3432456850837451385 ]") true;;

(* "|- (b ==> c) ==> a ==> (a ==> b) ==> c" *)

register_proof 1953624374286937666 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) b)) (v (bool) c))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) a)) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (v (bool) a)) (v (bool) b))) (v (bool) c))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !x y z. x + z = y + z <=> x = y" *)

register_proof 2821855162292404700 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) z) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) =) (a (a (c (fun (real) (fun (real) (real))) real_add) (v (real) x)) (v (real) z))) (a (a (c (fun (real) (fun (real) (real))) real_add) (v (real) y)) (v (real) z)))) (a (a (c (fun (real) (fun (real) (bool))) =) (v (real) x)) (v (real) y)))))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y. --x * y = --(x * y)" *)

register_proof 1053378900244349802 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (a (c (fun (real) (fun (real) (bool))) =) (a (a (c (fun (real) (fun (real) (real))) real_mul) (a (c (fun (real) (real)) real_neg) (v (real) x))) (v (real) y))) (a (c (fun (real) (real)) real_neg) (a (a (c (fun (real) (fun (real) (real))) real_mul) (v (real) x)) (v (real) y))))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y. --x <= --y <=> y <= x" *)

register_proof 2070457460911957643 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_le) (a (c (fun (real) (real)) real_neg) (v (real) x))) (a (c (fun (real) (real)) real_neg) (v (real) y)))) (a (a (c (fun (real) (fun (real) (bool))) real_le) (v (real) y)) (v (real) x)))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x. abs (--x) = abs x" *)

register_proof 3099096996048408987 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (a (c (fun (real) (fun (real) (bool))) =) (a (c (fun (real) (real)) real_abs) (a (c (fun (real) (real)) real_neg) (v (real) x)))) (a (c (fun (real) (real)) real_abs) (v (real) x)))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- (&m < -- &n <=> F) /\  (&m < &n <=> m < n) /\  (-- &m < -- &n <=> n < m) /\  (-- &m < &n <=> ~(m = 0 /\ n = 0))" *)

register_proof 2510951287326707397 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (num) (real)) real_of_num) (v (num) m))) (a (c (fun (real) (real)) real_neg) (a (c (fun (num) (real)) real_of_num) (v (num) n))))) (c (bool) F))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (num) (real)) real_of_num) (v (num) m))) (a (c (fun (num) (real)) real_of_num) (v (num) n)))) (a (a (c (fun (num) (fun (num) (bool))) <) (v (num) m)) (v (num) n)))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (real) (real)) real_neg) (a (c (fun (num) (real)) real_of_num) (v (num) m)))) (a (c (fun (real) (real)) real_neg) (a (c (fun (num) (real)) real_of_num) (v (num) n))))) (a (a (c (fun (num) (fun (num) (bool))) <) (v (num) n)) (v (num) m)))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (real) (real)) real_neg) (a (c (fun (num) (real)) real_of_num) (v (num) m)))) (a (c (fun (num) (real)) real_of_num) (v (num) n)))) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) m)) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (a (a (c (fun (num) (fun (num) (bool))) =) (v (num) n)) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 1374621388813277359 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3090688098468980513 ; THM 406147607937521369 ; THM 1107577190732069202 ; THM 1374621388813277359 ; THM 3936748912061086037 ]") true;;

(* "|- !x y. x < y \/ y <= x" *)

register_proof 3904160773221641559 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) x)) (v (real) y))) (a (a (c (fun (real) (fun (real) (bool))) real_le) (v (real) y)) (v (real) x)))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y. x < y ==> x <= y" *)

register_proof 2683684620265114838 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) x)) (v (real) y))) (a (a (c (fun (real) (fun (real) (bool))) real_le) (v (real) x)) (v (real) y)))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y. ~(x < y /\ y <= x)" *)

register_proof 644416155793463769 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) x)) (v (real) y))) (a (a (c (fun (real) (fun (real) (bool))) real_le) (v (real) y)) (v (real) x))))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y. ~(x < y) <=> y <= x" *)

register_proof 569731390285865421 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) x)) (v (real) y)))) (a (a (c (fun (real) (fun (real) (bool))) real_le) (v (real) y)) (v (real) x)))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y. &0 <= x /\ &0 < y ==> &0 < x + y" *)

register_proof 3459095117330726519 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (real) (fun (real) (bool))) real_le) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (real) x))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (real) y)))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (a (a (c (fun (real) (fun (real) (real))) real_add) (v (real) x)) (v (real) y))))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x. x * &1 = x" *)

register_proof 785495022541703514 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (a (c (fun (real) (fun (real) (bool))) =) (a (a (c (fun (real) (fun (real) (real))) real_mul) (v (real) x)) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT1) (c (num) _0)))))) (v (real) x))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- (x < y <=> y - x > &0) /\  (x <= y <=> y - x >= &0) /\  (x > y <=> x - y > &0) /\  (x >= y <=> x - y >= &0) /\  (x = y <=> x - y = &0) /\  (~(x < y) <=> x - y >= &0) /\  (~(x <= y) <=> x - y > &0) /\  (~(x > y) <=> y - x >= &0) /\  (~(x >= y) <=> y - x > &0) /\  (~(x = y) <=> x - y > &0 \/ --(x - y) > &0)" *)

register_proof 3516813337621061169 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) x)) (v (real) y))) (a (a (c (fun (real) (fun (real) (bool))) real_gt) (a (a (c (fun (real) (fun (real) (real))) real_sub) (v (real) y)) (v (real) x))) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_le) (v (real) x)) (v (real) y))) (a (a (c (fun (real) (fun (real) (bool))) real_ge) (a (a (c (fun (real) (fun (real) (real))) real_sub) (v (real) y)) (v (real) x))) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_gt) (v (real) x)) (v (real) y))) (a (a (c (fun (real) (fun (real) (bool))) real_gt) (a (a (c (fun (real) (fun (real) (real))) real_sub) (v (real) x)) (v (real) y))) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_ge) (v (real) x)) (v (real) y))) (a (a (c (fun (real) (fun (real) (bool))) real_ge) (a (a (c (fun (real) (fun (real) (real))) real_sub) (v (real) x)) (v (real) y))) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) =) (v (real) x)) (v (real) y))) (a (a (c (fun (real) (fun (real) (bool))) =) (a (a (c (fun (real) (fun (real) (real))) real_sub) (v (real) x)) (v (real) y))) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) x)) (v (real) y)))) (a (a (c (fun (real) (fun (real) (bool))) real_ge) (a (a (c (fun (real) (fun (real) (real))) real_sub) (v (real) x)) (v (real) y))) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (real) (fun (real) (bool))) real_le) (v (real) x)) (v (real) y)))) (a (a (c (fun (real) (fun (real) (bool))) real_gt) (a (a (c (fun (real) (fun (real) (real))) real_sub) (v (real) x)) (v (real) y))) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (real) (fun (real) (bool))) real_gt) (v (real) x)) (v (real) y)))) (a (a (c (fun (real) (fun (real) (bool))) real_ge) (a (a (c (fun (real) (fun (real) (real))) real_sub) (v (real) y)) (v (real) x))) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (real) (fun (real) (bool))) real_ge) (v (real) x)) (v (real) y)))) (a (a (c (fun (real) (fun (real) (bool))) real_gt) (a (a (c (fun (real) (fun (real) (real))) real_sub) (v (real) y)) (v (real) x))) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (real) (fun (real) (bool))) =) (v (real) x)) (v (real) y)))) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (a (c (fun (real) (fun (real) (bool))) real_gt) (a (a (c (fun (real) (fun (real) (real))) real_sub) (v (real) x)) (v (real) y))) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))) (a (a (c (fun (real) (fun (real) (bool))) real_gt) (a (c (fun (real) (real)) real_neg) (a (a (c (fun (real) (fun (real) (real))) real_sub) (v (real) x)) (v (real) y)))) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))))))))))))))",
    Parse_tactic.parse "REAL_ARITH_TAC") true;;

(* "|- !x. x * x >= &0" *)

register_proof 358641762956207037 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (a (c (fun (real) (fun (real) (bool))) real_ge) (a (a (c (fun (real) (fun (real) (real))) real_mul) (v (real) x)) (v (real) x))) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 2633191198328819870 ; THM 553978346798703924 ]") true;;

(* "|- !m n. &m >= &n <=> m >= n" *)

register_proof 439703399120693325 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_ge) (a (c (fun (num) (real)) real_of_num) (v (num) m))) (a (c (fun (num) (real)) real_of_num) (v (num) n)))) (a (a (c (fun (num) (fun (num) (bool))) >=) (v (num) m)) (v (num) n)))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 1519551676238234042 ]" THEN
    Parse_tactic.parse "MP_TAC THM 1637178899274243946" THEN
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- !m n. m <= n ==> &n - &m = &(n - m)" *)

register_proof 3708859984603535340 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) m) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (num) (fun (num) (bool))) <=) (v (num) m)) (v (num) n))) (a (a (c (fun (real) (fun (real) (bool))) =) (a (a (c (fun (real) (fun (real) (real))) real_sub) (a (c (fun (num) (real)) real_of_num) (v (num) n))) (a (c (fun (num) (real)) real_of_num) (v (num) m)))) (a (c (fun (num) (real)) real_of_num) (a (a (c (fun (num) (fun (num) (num))) -) (v (num) n)) (v (num) m)))))))))",
    Parse_tactic.parse "MP_TAC THM 406147607937521369" THEN
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- !x y. --(x * y) = --x * y" *)

register_proof 188099838346795975 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (a (c (fun (real) (fun (real) (bool))) =) (a (c (fun (real) (real)) real_neg) (a (a (c (fun (real) (fun (real) (real))) real_mul) (v (real) x)) (v (real) y)))) (a (a (c (fun (real) (fun (real) (real))) real_mul) (a (c (fun (real) (real)) real_neg) (v (real) x))) (v (real) y)))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y z. x + z < y + z <=> x < y" *)

register_proof 3105672383067826248 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) z) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (a (c (fun (real) (fun (real) (real))) real_add) (v (real) x)) (v (real) z))) (a (a (c (fun (real) (fun (real) (real))) real_add) (v (real) y)) (v (real) z)))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) x)) (v (real) y)))))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y. ~(x <= y /\ y < x)" *)

register_proof 312204931688278746 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (real) (fun (real) (bool))) real_le) (v (real) x)) (v (real) y))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) y)) (v (real) x))))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y. x = y \/ x < y \/ y < x" *)

register_proof 712432623580853146 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (a (c (fun (real) (fun (real) (bool))) =) (v (real) x)) (v (real) y))) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) x)) (v (real) y))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) y)) (v (real) x))))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y z. x + z <= y + z <=> x <= y" *)

register_proof 3335411181795662436 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) z) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_le) (a (a (c (fun (real) (fun (real) (real))) real_add) (v (real) x)) (v (real) z))) (a (a (c (fun (real) (fun (real) (real))) real_add) (v (real) y)) (v (real) z)))) (a (a (c (fun (real) (fun (real) (bool))) real_le) (v (real) x)) (v (real) y)))))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y z. y < x + --z <=> y + z < x" *)

register_proof 1668258396541408383 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) z) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) y)) (a (a (c (fun (real) (fun (real) (real))) real_add) (v (real) x)) (a (c (fun (real) (real)) real_neg) (v (real) z))))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (a (c (fun (real) (fun (real) (real))) real_add) (v (real) y)) (v (real) z))) (v (real) x)))))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x. x - x = &0" *)

register_proof 1208943952793503948 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (a (c (fun (real) (fun (real) (bool))) =) (a (a (c (fun (real) (fun (real) (real))) real_sub) (v (real) x)) (v (real) x))) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))))",
    Parse_tactic.parse "REAL_ARITH_TAC") true;;

(* "|- !x y. (x + y) - x = y" *)

register_proof 1781258313293611011 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (a (c (fun (real) (fun (real) (bool))) =) (a (a (c (fun (real) (fun (real) (real))) real_sub) (a (a (c (fun (real) (fun (real) (real))) real_add) (v (real) x)) (v (real) y))) (v (real) x))) (v (real) y))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y. y <= x + y <=> &0 <= x" *)

register_proof 1894400040801942334 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_le) (v (real) y)) (a (a (c (fun (real) (fun (real) (real))) real_add) (v (real) x)) (v (real) y)))) (a (a (c (fun (real) (fun (real) (bool))) real_le) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (real) x)))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y z. x - y < z <=> x < z + y" *)

register_proof 869535500521353633 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) z) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (a (c (fun (real) (fun (real) (real))) real_sub) (v (real) x)) (v (real) y))) (v (real) z))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) x)) (a (a (c (fun (real) (fun (real) (real))) real_add) (v (real) z)) (v (real) y))))))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !w x y z. w < x /\ y <= z ==> w + y < x + z" *)

register_proof 2386747048803691417 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) w) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) z) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) w)) (v (real) x))) (a (a (c (fun (real) (fun (real) (bool))) real_le) (v (real) y)) (v (real) z)))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (a (c (fun (real) (fun (real) (real))) real_add) (v (real) w)) (v (real) y))) (a (a (c (fun (real) (fun (real) (real))) real_add) (v (real) x)) (v (real) z))))))))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y z. x = y - z <=> x + z = y" *)

register_proof 3714350038189073359 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) z) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) =) (v (real) x)) (a (a (c (fun (real) (fun (real) (real))) real_sub) (v (real) y)) (v (real) z)))) (a (a (c (fun (real) (fun (real) (bool))) =) (a (a (c (fun (real) (fun (real) (real))) real_add) (v (real) x)) (v (real) z))) (v (real) y)))))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y z. (x - y) * z = x * z - y * z" *)

register_proof 216261582596864702 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) z) (a (a (c (fun (real) (fun (real) (bool))) =) (a (a (c (fun (real) (fun (real) (real))) real_mul) (a (a (c (fun (real) (fun (real) (real))) real_sub) (v (real) x)) (v (real) y))) (v (real) z))) (a (a (c (fun (real) (fun (real) (real))) real_sub) (a (a (c (fun (real) (fun (real) (real))) real_mul) (v (real) x)) (v (real) z))) (a (a (c (fun (real) (fun (real) (real))) real_mul) (v (real) y)) (v (real) z))))))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y z. abs x + abs (y - x) <= z ==> abs y <= z" *)

register_proof 662472977331870928 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) z) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (real) (fun (real) (bool))) real_le) (a (a (c (fun (real) (fun (real) (real))) real_add) (a (c (fun (real) (real)) real_abs) (v (real) x))) (a (c (fun (real) (real)) real_abs) (a (a (c (fun (real) (fun (real) (real))) real_sub) (v (real) y)) (v (real) x))))) (v (real) z))) (a (a (c (fun (real) (fun (real) (bool))) real_le) (a (c (fun (real) (real)) real_abs) (v (real) y))) (v (real) z)))))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x. abs (abs x) = abs x" *)

register_proof 2834163578412299518 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (a (c (fun (real) (fun (real) (bool))) =) (a (c (fun (real) (real)) real_abs) (a (c (fun (real) (real)) real_abs) (v (real) x)))) (a (c (fun (real) (real)) real_abs) (v (real) x)))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1085826896465312698 ; THM 1801071607812926126 ]") true;;

(* "|- !x y. abs (x - y) < abs y ==> ~(x = &0)" *)

register_proof 3537397687142893939 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (real) (real)) real_abs) (a (a (c (fun (real) (fun (real) (real))) real_sub) (v (real) x)) (v (real) y)))) (a (c (fun (real) (real)) real_abs) (v (real) y)))) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (real) (fun (real) (bool))) =) (v (real) x)) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y h. abs h < abs y - abs x ==> abs (x + h) < abs y" *)

register_proof 1272658363554642587 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) h) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (real) (real)) real_abs) (v (real) h))) (a (a (c (fun (real) (fun (real) (real))) real_sub) (a (c (fun (real) (real)) real_abs) (v (real) y))) (a (c (fun (real) (real)) real_abs) (v (real) x))))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (real) (real)) real_abs) (a (a (c (fun (real) (fun (real) (real))) real_add) (v (real) x)) (v (real) h)))) (a (c (fun (real) (real)) real_abs) (v (real) y))))))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x k. --k <= x /\ x <= k <=> abs x <= k" *)

register_proof 2620547961580522022 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) k) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (real) (fun (real) (bool))) real_le) (a (c (fun (real) (real)) real_neg) (v (real) k))) (v (real) x))) (a (a (c (fun (real) (fun (real) (bool))) real_le) (v (real) x)) (v (real) k)))) (a (a (c (fun (real) (fun (real) (bool))) real_le) (a (c (fun (real) (real)) real_abs) (v (real) x))) (v (real) k)))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y. min x y <= x /\ min x y <= y" *)

register_proof 1746106422450125990 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (real) (fun (real) (bool))) real_le) (a (a (c (fun (real) (fun (real) (real))) real_min) (v (real) x)) (v (real) y))) (v (real) x))) (a (a (c (fun (real) (fun (real) (bool))) real_le) (a (a (c (fun (real) (fun (real) (real))) real_min) (v (real) x)) (v (real) y))) (v (real) y)))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y z. z < max x y <=> z < x \/ z < y" *)

register_proof 4376051185621137800 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) z) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) z)) (a (a (c (fun (real) (fun (real) (real))) real_max) (v (real) x)) (v (real) y)))) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) z)) (v (real) x))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) z)) (v (real) y))))))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y z. min x y < z <=> x < z \/ y < z" *)

register_proof 2230548334482098180 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) z) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (a (c (fun (real) (fun (real) (real))) real_min) (v (real) x)) (v (real) y))) (v (real) z))) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) x)) (v (real) z))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) y)) (v (real) z))))))))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y z. x <= y /\ &0 <= z ==> x * z <= y * z" *)

register_proof 2656803499285990198 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) z) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (real) (fun (real) (bool))) real_le) (v (real) x)) (v (real) y))) (a (a (c (fun (real) (fun (real) (bool))) real_le) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (real) z)))) (a (a (c (fun (real) (fun (real) (bool))) real_le) (a (a (c (fun (real) (fun (real) (real))) real_mul) (v (real) x)) (v (real) z))) (a (a (c (fun (real) (fun (real) (real))) real_mul) (v (real) y)) (v (real) z))))))))))",
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 1519551676238234042 ]" THEN
    Parse_tactic.parse "MP_TAC THM 2633191198328819870" THEN
    Parse_tactic.parse "DISCH_TAC" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1672658611913439754 ; THM 2305091872219473420 ; THM 4394545264305412821 ]") true;;

(* "|- !x y. inv (x / y) = y / x" *)

register_proof 563567935943302785 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (a (c (fun (real) (fun (real) (bool))) =) (a (c (fun (real) (real)) real_inv) (a (a (c (fun (real) (fun (real) (real))) real_div) (v (real) x)) (v (real) y)))) (a (a (c (fun (real) (fun (real) (real))) real_div) (v (real) y)) (v (real) x)))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 2738361808698163958 ; THM 1672658611913439754 ; THM 2943680762235744116 ; THM 2724438577850146690 ]") true;;

(* "|- !x y z. &0 < x /\ x * y < x * z ==> y < z" *)

register_proof 4277731856012452112 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) z) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (real) x))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (a (c (fun (real) (fun (real) (real))) real_mul) (v (real) x)) (v (real) y))) (a (a (c (fun (real) (fun (real) (real))) real_mul) (v (real) x)) (v (real) z))))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) y)) (v (real) z)))))))))",
    Parse_tactic.parse "MP_TAC THM 749187894152448218" THEN
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 1041278397167009944 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3936748912061086037 ; THM 501843349443172382 ]") true;;

(* "|- !x y z. &0 < z ==> (z * x <= z * y <=> x <= y)" *)

register_proof 3199725509771310843 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) z) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (real) z))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_le) (a (a (c (fun (real) (fun (real) (real))) real_mul) (v (real) z)) (v (real) x))) (a (a (c (fun (real) (fun (real) (real))) real_mul) (v (real) z)) (v (real) y)))) (a (a (c (fun (real) (fun (real) (bool))) real_le) (v (real) x)) (v (real) y))))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 749187894152448218 ; THM 3936748912061086037 ; THM 4277731856012452112 ]") true;;

(* "|- !x y z. &0 < z ==> (x / z < y <=> x < y * z)" *)

register_proof 1583765991722158477 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) z) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (real) z))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (a (c (fun (real) (fun (real) (real))) real_div) (v (real) x)) (v (real) z))) (v (real) y))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) x)) (a (a (c (fun (real) (fun (real) (real))) real_mul) (v (real) y)) (v (real) z)))))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3936748912061086037 ; THM 4456295163615992590 ]") true;;

(* "|- !x. &2 * x = x + x" *)

register_proof 1466630993683248374 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (a (c (fun (real) (fun (real) (bool))) =) (a (a (c (fun (real) (fun (real) (real))) real_mul) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT0) (a (c (fun (num) (num)) BIT1) (c (num) _0)))))) (v (real) x))) (a (a (c (fun (real) (fun (real) (real))) real_add) (v (real) x)) (v (real) x)))))",
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x y. &0 < x /\ x <= y ==> inv y <= inv x" *)

register_proof 1088268366015112389 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (real) x))) (a (a (c (fun (real) (fun (real) (bool))) real_le) (v (real) x)) (v (real) y)))) (a (a (c (fun (real) (fun (real) (bool))) real_le) (a (c (fun (real) (real)) real_inv) (v (real) y))) (a (c (fun (real) (real)) real_inv) (v (real) x))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1601937691945011183 ; THM 501843349443172382 ]") true;;

(* "|- !x. &1 <= x ==> inv x <= &1" *)

register_proof 2972866958123163503 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (real) (fun (real) (bool))) real_le) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT1) (c (num) _0))))) (v (real) x))) (a (a (c (fun (real) (fun (real) (bool))) real_le) (a (c (fun (real) (real)) real_inv) (v (real) x))) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT1) (c (num) _0))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1384567224173581798 ; THM 4472725768618010415 ; THM 1088268366015112389 ]") true;;

(* "|- !x y. &0 < x /\ &0 < y ==> &0 < x / y" *)

register_proof 193041646609073416 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (real) x))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (real) y)))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (a (a (c (fun (real) (fun (real) (real))) real_div) (v (real) x)) (v (real) y))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 2738361808698163958 ; THM 1743090393200252989 ; THM 4452873421319066623 ]") true;;

(* "|- !n. &1 <= &2 pow n" *)

register_proof 1287927275600963244 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (a (c (fun (real) (fun (real) (bool))) real_le) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT1) (c (num) _0))))) (a (a (c (fun (real) (fun (num) (real))) real_pow) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT0) (a (c (fun (num) (num)) BIT1) (c (num) _0)))))) (v (num) n)))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 501843349443172382 ]" THEN
    Parse_tactic.parse "MP_TAC THM 2724438577850146690" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 2724438577850146690 ; THM 1664447345590917620 ]" THEN
    Parse_tactic.parse "MP_TAC THM 827836301053369836" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 501843349443172382 ; THM 3007232846364719613 ; THM 1664447345590917620 ; THM 406147607937521369 ; THM 3315800900687253718 ; THM 3286201379011122149 ; THM 392922024883520428 ; THM 2179929019602649085 ; THM 62096996627259201 ; THM 2214215657264864531 ]") true;;

(* "|- !x. &0 <= x pow 2" *)

register_proof 2885360789513008724 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (a (c (fun (real) (fun (real) (bool))) real_le) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (a (a (c (fun (real) (fun (num) (real))) real_pow) (v (real) x)) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT0) (a (c (fun (num) (num)) BIT1) (c (num) _0))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3622580955900097453 ; THM 2633191198328819870 ]") true;;

(* "|- !n x y. ODD n ==> (x pow n = y pow n <=> x = y)" *)

register_proof 2570701819462424600 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (c (fun (num) (bool)) ODD) (v (num) n))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) =) (a (a (c (fun (real) (fun (num) (real))) real_pow) (v (real) x)) (v (num) n))) (a (a (c (fun (real) (fun (num) (real))) real_pow) (v (real) y)) (v (num) n)))) (a (a (c (fun (real) (fun (real) (bool))) =) (v (real) x)) (v (real) y))))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 2034092285640598369 ; THM 3936748912061086037 ; THM 501843349443172382 ]") true;;

(* "|- !x. ?n. x < &n" *)

register_proof 3769496234385949574 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (num) (bool)) (bool)) ?) (l (v (num) n) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) x)) (a (c (fun (num) (real)) real_of_num) (v (num) n)))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 4429544143608775554 ; THM 3315800900687253718 ; THM 1802706013665385295 ; THM 569731390285865421 ; THM 2137115981466194412 ]") true;;

(* "|- !x. real_sgn x * x = abs x" *)

register_proof 2066333626372145526 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (a (c (fun (real) (fun (real) (bool))) =) (a (a (c (fun (real) (fun (real) (real))) real_mul) (a (c (fun (real) (real)) real_sgn) (v (real) x))) (v (real) x))) (a (c (fun (real) (real)) real_abs) (v (real) x)))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 1672658611913439754 ; THM 133988277336422298 ]" THEN
    Parse_tactic.parse "REAL_ARITH_TAC2") true;;

(* "|- !x. real_sgn (inv x) = real_sgn x" *)

register_proof 435830261999595958 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (a (c (fun (real) (fun (real) (bool))) =) (a (c (fun (real) (real)) real_sgn) (a (c (fun (real) (real)) real_inv) (v (real) x)))) (a (c (fun (real) (real)) real_sgn) (v (real) x)))))",
    Parse_tactic.parse "MP_TAC THM 133988277336422298" THEN
    Parse_tactic.parse "SIMP_TAC [ ]" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 3936748912061086037 ; THM 3688701048670243857 ; THM 2834119410112381607 ]") true;;

(* "|- !x y.    real_sgn x = real_sgn y <=>    (x = &0 <=> y = &0) /\ (x > &0 <=> y > &0) /\ (x < &0 <=> y < &0)" *)

register_proof 1403634932299303587 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) =) (a (c (fun (real) (real)) real_sgn) (v (real) x))) (a (c (fun (real) (real)) real_sgn) (v (real) y)))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) =) (v (real) x)) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))) (a (a (c (fun (real) (fun (real) (bool))) =) (v (real) y)) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_gt) (v (real) x)) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))) (a (a (c (fun (real) (fun (real) (bool))) real_gt) (v (real) y)) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) x)) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) y)) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))))))))))",
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 4316752623609269416 ]" THEN
    Parse_tactic.parse "MP_TAC THM 1519551676238234042" THEN
    Parse_tactic.parse "DISCH_TAC" THEN
    Parse_tactic.parse "MP_TAC THM 1519551676238234042" THEN
    Parse_tactic.parse "DISCH_TAC" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3936748912061086037 ; THM 1227369047447041727 ; THM 3700168858861150758 ; THM 133988277336422298 ; THM 501843349443172382 ; THM 2683027776552938252 ; THM 2012640948690278230 ; THM 1564373864581257982 ; THM 2811381786271539603 ; THM 183126649061856388 ; THM 1122139855026223720 ]") true;;

(* "|- &0 < y1 /\ &0 < y2  ==> x1 / y1 + x2 / y2 = (x1 * y2 + x2 * y1) * inv y1 * inv y2" *)

register_proof 519800824368622826 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (real) y1))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (real) y2)))) (a (a (c (fun (real) (fun (real) (bool))) =) (a (a (c (fun (real) (fun (real) (real))) real_add) (a (a (c (fun (real) (fun (real) (real))) real_div) (v (real) x1)) (v (real) y1))) (a (a (c (fun (real) (fun (real) (real))) real_div) (v (real) x2)) (v (real) y2)))) (a (a (c (fun (real) (fun (real) (real))) real_mul) (a (a (c (fun (real) (fun (real) (real))) real_add) (a (a (c (fun (real) (fun (real) (real))) real_mul) (v (real) x1)) (v (real) y2))) (a (a (c (fun (real) (fun (real) (real))) real_mul) (v (real) x2)) (v (real) y1)))) (a (a (c (fun (real) (fun (real) (real))) real_mul) (a (c (fun (real) (real)) real_inv) (v (real) y1))) (a (c (fun (real) (real)) real_inv) (v (real) y2))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3330005187880438220 ; THM 2043248966121458867 ]") true;;

(* "|- &0 < y1 /\ &0 < y2 ==> (x1 / y1 = x2 / y2 <=> x1 * y2 = x2 * y1)" *)

register_proof 3708228548239114346 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (real) y1))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (real) y2)))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) =) (a (a (c (fun (real) (fun (real) (real))) real_div) (v (real) x1)) (v (real) y1))) (a (a (c (fun (real) (fun (real) (real))) real_div) (v (real) x2)) (v (real) y2)))) (a (a (c (fun (real) (fun (real) (bool))) =) (a (a (c (fun (real) (fun (real) (real))) real_mul) (v (real) x1)) (v (real) y2))) (a (a (c (fun (real) (fun (real) (real))) real_mul) (v (real) x2)) (v (real) y1)))))",
    Parse_tactic.parse "DISCH_TAC" THEN
    Parse_tactic.parse "EQ_TAC" THEN
    Parse_tactic.parse "MP_TAC THM 897231848567675259" THEN
    Parse_tactic.parse "DISCH_TAC" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 897231848567675259 ; THM 501843349443172382 ; THM 1169672806807290955 ]" THEN
    Parse_tactic.parse "MP_TAC THM 897231848567675259" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 897231848567675259 ; THM 501843349443172382 ; THM 3936748912061086037 ]") true;;

(* "|- &0 < y1 ==> &0 < y2 ==> (x1 / y1 = x2 / y2 <=> x1 * y2 = x2 * y1)" *)

register_proof 4362678427624993842 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (real) y1))) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (a (c (fun (num) (real)) real_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (real) y2))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) =) (a (a (c (fun (real) (fun (real) (real))) real_div) (v (real) x1)) (v (real) y1))) (a (a (c (fun (real) (fun (real) (real))) real_div) (v (real) x2)) (v (real) y2)))) (a (a (c (fun (real) (fun (real) (bool))) =) (a (a (c (fun (real) (fun (real) (real))) real_mul) (v (real) x1)) (v (real) y2))) (a (a (c (fun (real) (fun (real) (real))) real_mul) (v (real) x2)) (v (real) y1))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3708228548239114346 ]") true;;

(* "|- (x / y) pow n = x pow n / y pow n" *)

register_proof 1201646350700214575 (
  fun () ->
    decode_goal [] "(a (a (c (fun (real) (fun (real) (bool))) =) (a (a (c (fun (real) (fun (num) (real))) real_pow) (a (a (c (fun (real) (fun (real) (real))) real_div) (v (real) x)) (v (real) y))) (v (num) n))) (a (a (c (fun (real) (fun (real) (real))) real_div) (a (a (c (fun (real) (fun (num) (real))) real_pow) (v (real) x)) (v (num) n))) (a (a (c (fun (real) (fun (num) (real))) real_pow) (v (real) y)) (v (num) n))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 979019804724911848 ]") true;;

(* "|- !x y. x <= y <=> (!z. y < z ==> x < z)" *)

register_proof 3759206844334411729 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) x) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) y) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (real) (fun (real) (bool))) real_le) (v (real) x)) (v (real) y))) (a (c (fun (fun (real) (bool)) (bool)) !) (l (v (real) z) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) y)) (v (real) z))) (a (a (c (fun (real) (fun (real) (bool))) real_lt) (v (real) x)) (v (real) z))))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3936748912061086037 ; THM 501843349443172382 ; THM 4493349066701814631 ]") true;;

(* "|- !i. ?n. real_of_int i = &n \/ real_of_int i = -- &n" *)

register_proof 4440698131740990629 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (int) (bool)) (bool)) !) (l (v (int) i) (a (c (fun (fun (num) (bool)) (bool)) ?) (l (v (num) n) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (a (c (fun (real) (fun (real) (bool))) =) (a (c (fun (int) (real)) real_of_int) (v (int) i))) (a (c (fun (num) (real)) real_of_num) (v (num) n)))) (a (a (c (fun (real) (fun (real) (bool))) =) (a (c (fun (int) (real)) real_of_int) (v (int) i))) (a (c (fun (real) (real)) real_neg) (a (c (fun (num) (real)) real_of_num) (v (num) n)))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1707962672990974409 ; THM 2285865211877052620 ]") true;;

(* "|- !x y. real_of_int (x + y) = real_of_int x + real_of_int y" *)

register_proof 3166949747580149816 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (int) (bool)) (bool)) !) (l (v (int) x) (a (c (fun (fun (int) (bool)) (bool)) !) (l (v (int) y) (a (a (c (fun (real) (fun (real) (bool))) =) (a (c (fun (int) (real)) real_of_int) (a (a (c (fun (int) (fun (int) (int))) int_add) (v (int) x)) (v (int) y)))) (a (a (c (fun (real) (fun (real) (real))) real_add) (a (c (fun (int) (real)) real_of_int) (v (int) x))) (a (c (fun (int) (real)) real_of_int) (v (int) y))))))))",
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- !x y. real_of_int (max x y) = max (real_of_int x) (real_of_int y)" *)

register_proof 3254220618102278701 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (int) (bool)) (bool)) !) (l (v (int) x) (a (c (fun (fun (int) (bool)) (bool)) !) (l (v (int) y) (a (a (c (fun (real) (fun (real) (bool))) =) (a (c (fun (int) (real)) real_of_int) (a (a (c (fun (int) (fun (int) (int))) int_max) (v (int) x)) (v (int) y)))) (a (a (c (fun (real) (fun (real) (real))) real_max) (a (c (fun (int) (real)) real_of_int) (v (int) x))) (a (c (fun (int) (real)) real_of_int) (v (int) y))))))))",
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- !x y. x > y <=> x >= y + &1" *)

register_proof 1010265619805557563 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (int) (bool)) (bool)) !) (l (v (int) x) (a (c (fun (fun (int) (bool)) (bool)) !) (l (v (int) y) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (int) (fun (int) (bool))) int_gt) (v (int) x)) (v (int) y))) (a (a (c (fun (int) (fun (int) (bool))) int_ge) (v (int) x)) (a (a (c (fun (int) (fun (int) (int))) int_add) (v (int) y)) (a (c (fun (num) (int)) int_of_num) (a (c (fun (num) (num)) NUMERAL) (a (c (fun (num) (num)) BIT1) (c (num) _0)))))))))))",
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- !x y. x < y <=> ~(y <= x)" *)

register_proof 648883982436455055 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (int) (bool)) (bool)) !) (l (v (int) x) (a (c (fun (fun (int) (bool)) (bool)) !) (l (v (int) y) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (int) (fun (int) (bool))) int_lt) (v (int) x)) (v (int) y))) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (int) (fun (int) (bool))) int_le) (v (int) y)) (v (int) x))))))))",
    Parse_tactic.parse "ARITH_TAC") true;;

(* "|- !x y. x <= y <=> (!z. y < z ==> x < z)" *)

register_proof 4111090027468147373 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (int) (bool)) (bool)) !) (l (v (int) x) (a (c (fun (fun (int) (bool)) (bool)) !) (l (v (int) y) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (int) (fun (int) (bool))) int_le) (v (int) x)) (v (int) y))) (a (c (fun (fun (int) (bool)) (bool)) !) (l (v (int) z) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (int) (fun (int) (bool))) int_lt) (v (int) y)) (v (int) z))) (a (a (c (fun (int) (fun (int) (bool))) int_lt) (v (int) x)) (v (int) z))))))))))",
    Parse_tactic.parse "MP_TAC THM 3759206844334411729" THEN
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 1041278397167009944 ]" THEN
    Parse_tactic.parse "MP_TAC THM 1124271433710135282" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3935094352244413701 ; THM 648883982436455055 ]") true;;

(* "|- &0 * &x = &0 /\ &0 * -- &x = &0 /\ &x * &0 = &0 /\ -- &x * &0 = &0" *)

register_proof 2708175215262626735 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (int) (fun (int) (bool))) =) (a (a (c (fun (int) (fun (int) (int))) int_mul) (a (c (fun (num) (int)) int_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (a (c (fun (num) (int)) int_of_num) (v (num) x)))) (a (c (fun (num) (int)) int_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (int) (fun (int) (bool))) =) (a (a (c (fun (int) (fun (int) (int))) int_mul) (a (c (fun (num) (int)) int_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (a (c (fun (int) (int)) int_neg) (a (c (fun (num) (int)) int_of_num) (v (num) x))))) (a (c (fun (num) (int)) int_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (int) (fun (int) (bool))) =) (a (a (c (fun (int) (fun (int) (int))) int_mul) (a (c (fun (num) (int)) int_of_num) (v (num) x))) (a (c (fun (num) (int)) int_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))) (a (c (fun (num) (int)) int_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))) (a (a (c (fun (int) (fun (int) (bool))) =) (a (a (c (fun (int) (fun (int) (int))) int_mul) (a (c (fun (int) (int)) int_neg) (a (c (fun (num) (int)) int_of_num) (v (num) x)))) (a (c (fun (num) (int)) int_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0))))) (a (c (fun (num) (int)) int_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 4041010840155287118 ; THM 3929174669591841215 ; THM 936705270425624738 ; THM 2716585862233623666 ; THM 1672658611913439754 ; THM 1374621388813277359 ; THM 183126649061856388 ]" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 2604606682365368099 ; THM 4474993993933191654 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1374621388813277359 ; THM 2785399557515712619 ]") true;;

(* "|- (if T then x else y) = x /\ (if F then x else y) = y" *)

register_proof 4165416771054115050 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (int) (fun (int) (bool))) =) (a (a (a (c (fun (bool) (fun (int) (fun (int) (int)))) COND) (c (bool) T)) (v (int) x)) (v (int) y))) (v (int) x))) (a (a (c (fun (int) (fun (int) (bool))) =) (a (a (a (c (fun (bool) (fun (int) (fun (int) (int)))) COND) (c (bool) F)) (v (int) x)) (v (int) y))) (v (int) y)))",
    Parse_tactic.parse "REWRITE_TAC [ ]") true;;

(* "|- !x. &0 <= x <=> &(num_of_int x) = x" *)

register_proof 839023703392461294 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (int) (bool)) (bool)) !) (l (v (int) x) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (int) (fun (int) (bool))) int_le) (a (c (fun (num) (int)) int_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (int) x))) (a (a (c (fun (int) (fun (int) (bool))) =) (a (c (fun (num) (int)) int_of_num) (a (c (fun (int) (num)) num_of_int) (v (int) x)))) (v (int) x)))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 2177425220599037027 ; THM 1339447117273361433 ]") true;;

(* "|- ((!n. P (&n)) <=> (!i. &0 <= i ==> P i)) /\  ((?n. P (&n)) <=> (?i. &0 <= i /\ P i))" *)

register_proof 3481174769768973473 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun (num) (bool)) (bool)) !) (l (v (num) n) (a (v (fun (int) (bool)) P) (a (c (fun (num) (int)) int_of_num) (v (num) n)))))) (a (c (fun (fun (int) (bool)) (bool)) !) (l (v (int) i) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (int) (fun (int) (bool))) int_le) (a (c (fun (num) (int)) int_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (int) i))) (a (v (fun (int) (bool)) P) (v (int) i))))))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun (num) (bool)) (bool)) ?) (l (v (num) n) (a (v (fun (int) (bool)) P) (a (c (fun (num) (int)) int_of_num) (v (num) n)))))) (a (c (fun (fun (int) (bool)) (bool)) ?) (l (v (int) i) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (int) (fun (int) (bool))) int_le) (a (c (fun (num) (int)) int_of_num) (a (c (fun (num) (num)) NUMERAL) (c (num) _0)))) (v (int) i))) (a (v (fun (int) (bool)) P) (v (int) i)))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3460964253965123276 ; THM 4082512353770383665 ]") true;;

(* "|- !s x. x IN UNIONS s <=> (?t. t IN s /\ x IN t)" *)

register_proof 17050148103884248 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun (fun A (bool)) (bool)) (bool)) (bool)) !) (l (v (fun (fun A (bool)) (bool)) s) (a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun A (fun (fun A (bool)) (bool))) IN) (v A x)) (a (c (fun (fun (fun A (bool)) (bool)) (fun A (bool))) UNIONS) (v (fun (fun A (bool)) (bool)) s)))) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) ?) (l (v (fun A (bool)) t) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (fun A (bool)) (fun (fun (fun A (bool)) (bool)) (bool))) IN) (v (fun A (bool)) t)) (v (fun (fun A (bool)) (bool)) s))) (a (a (c (fun A (fun (fun A (bool)) (bool))) IN) (v A x)) (v (fun A (bool)) t))))))))))",
    Parse_tactic.parse "MP_TAC THM 1433978518111830493" THEN
    Parse_tactic.parse "SIMP_TAC [ ]" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 1041278397167009944 ; THM 1433978518111830493 ; THM 469727402399097348 ; THM 4432927027650125967 ; THM 2734571354838088458 ; THM 421915263640211727 ; THM 2525030685148683581 ; THM 3158068480668451995 ; THM 1204176519545196070 ; THM 2683027776552938252 ]" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 3988739980816549280 ; THM 2811381786271539603 ; THM 2683027776552938252 ; THM 1564373864581257982 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !s x y. x IN s DELETE y <=> x IN s /\ ~(x = y)" *)

register_proof 1617287271715630323 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (c (fun (fun A (bool)) (bool)) !) (l (v A y) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun A (fun (fun A (bool)) (bool))) IN) (v A x)) (a (a (c (fun (fun A (bool)) (fun A (fun A (bool)))) DELETE) (v (fun A (bool)) s)) (v A y)))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun A (fun (fun A (bool)) (bool))) IN) (v A x)) (v (fun A (bool)) s))) (a (c (fun (bool) (bool)) ~) (a (a (c (fun A (fun A (bool))) =) (v A x)) (v A y)))))))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 1433978518111830493 ; THM 1564373864581257982 ; THM 1376343245201252232 ; THM 1519551676238234042 ; THM 421915263640211727 ; THM 1298889241945060630 ; THM 2076440368235384317 ; THM 2525030685148683581 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !P a s. (?x. x IN a INSERT s /\ P x) <=> P a \/ (?x. x IN s /\ P x)" *)

register_proof 1551532558876655321 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun ?54437 (bool)) (bool)) (bool)) !) (l (v (fun ?54437 (bool)) P) (a (c (fun (fun ?54437 (bool)) (bool)) !) (l (v ?54437 a) (a (c (fun (fun (fun ?54437 (bool)) (bool)) (bool)) !) (l (v (fun ?54437 (bool)) s) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun ?54437 (bool)) (bool)) ?) (l (v ?54437 x) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun ?54437 (fun (fun ?54437 (bool)) (bool))) IN) (v ?54437 x)) (a (a (c (fun ?54437 (fun (fun ?54437 (bool)) (fun ?54437 (bool)))) INSERT) (v ?54437 a)) (v (fun ?54437 (bool)) s)))) (a (v (fun ?54437 (bool)) P) (v ?54437 x)))))) (a (a (c (fun (bool) (fun (bool) (bool))) \/) (a (v (fun ?54437 (bool)) P) (v ?54437 a))) (a (c (fun (fun ?54437 (bool)) (bool)) ?) (l (v ?54437 x) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun ?54437 (fun (fun ?54437 (bool)) (bool))) IN) (v ?54437 x)) (v (fun ?54437 (bool)) s))) (a (v (fun ?54437 (bool)) P) (v ?54437 x)))))))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 463448412851097228 ]") true;;

(* "|- !s. (?x. x IN s) <=> ~(s = {})" *)

register_proof 2836278895582002928 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun A (bool)) (bool)) ?) (l (v A x) (a (a (c (fun A (fun (fun A (bool)) (bool))) IN) (v A x)) (v (fun A (bool)) s))))) (a (c (fun (bool) (bool)) ~) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) =) (v (fun A (bool)) s)) (c (fun A (bool)) EMPTY))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 4495262615029895189 ; THM 2527016271037755868 ]") true;;

(* "|- !s. s SUBSET s" *)

register_proof 3806712188414811372 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) SUBSET) (v (fun A (bool)) s)) (v (fun A (bool)) s))))",
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 3713350661807540262 ]" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 3600758243849386141 ]") true;;

(* "|- !s. s SUBSET (:A)" *)

register_proof 1047354489322248470 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) SUBSET) (v (fun A (bool)) s)) (c (fun A (bool)) UNIV))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3600758243849386141 ; THM 3311463869563081269 ]") true;;

(* "|- !s t u. s PSUBSET t /\ t SUBSET u ==> s PSUBSET u" *)

register_proof 272255229214645852 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) t) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) u) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) PSUBSET) (v (fun A (bool)) s)) (v (fun A (bool)) t))) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) SUBSET) (v (fun A (bool)) t)) (v (fun A (bool)) u)))) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) PSUBSET) (v (fun A (bool)) s)) (v (fun A (bool)) u)))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 4337520006400750416 ; THM 3200010706713031938 ]") true;;

(* "|- !s. s PSUBSET (:A) <=> (?x. ~(x IN s))" *)

register_proof 1409998405573405081 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) PSUBSET) (v (fun A (bool)) s)) (c (fun A (bool)) UNIV))) (a (c (fun (fun A (bool)) (bool)) ?) (l (v A x) (a (c (fun (bool) (bool)) ~) (a (a (c (fun A (fun (fun A (bool)) (bool))) IN) (v A x)) (v (fun A (bool)) s))))))))",
    Parse_tactic.parse "MP_TAC THM 3600758243849386141" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 2527016271037755868 ; THM 3311463869563081269 ; THM 4337520006400750416 ]") true;;

(* "|- (!s t. s SUBSET s UNION t) /\ (!s t. s SUBSET t UNION s)" *)

register_proof 45650406631493068 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) t) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) SUBSET) (v (fun A (bool)) s)) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (fun A (bool)))) UNION) (v (fun A (bool)) s)) (v (fun A (bool)) t)))))))) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) t) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) SUBSET) (v (fun A (bool)) s)) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (fun A (bool)))) UNION) (v (fun A (bool)) t)) (v (fun A (bool)) s))))))))",
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 4333020478253274202 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3600758243849386141 ; THM 975856962495483397 ]") true;;

(* "|- !s t u. s UNION t SUBSET u <=> s SUBSET u /\ t SUBSET u" *)

register_proof 3684008257334774403 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun ?55173 (bool)) (bool)) (bool)) !) (l (v (fun ?55173 (bool)) s) (a (c (fun (fun (fun ?55173 (bool)) (bool)) (bool)) !) (l (v (fun ?55173 (bool)) t) (a (c (fun (fun (fun ?55173 (bool)) (bool)) (bool)) !) (l (v (fun ?55173 (bool)) u) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (fun ?55173 (bool)) (fun (fun ?55173 (bool)) (bool))) SUBSET) (a (a (c (fun (fun ?55173 (bool)) (fun (fun ?55173 (bool)) (fun ?55173 (bool)))) UNION) (v (fun ?55173 (bool)) s)) (v (fun ?55173 (bool)) t))) (v (fun ?55173 (bool)) u))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (fun ?55173 (bool)) (fun (fun ?55173 (bool)) (bool))) SUBSET) (v (fun ?55173 (bool)) s)) (v (fun ?55173 (bool)) u))) (a (a (c (fun (fun ?55173 (bool)) (fun (fun ?55173 (bool)) (bool))) SUBSET) (v (fun ?55173 (bool)) t)) (v (fun ?55173 (bool)) u))))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3600758243849386141 ; THM 975856962495483397 ]") true;;

(* "|- !s. s INTER s = s" *)

register_proof 2921091475408260770 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) =) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (fun A (bool)))) INTER) (v (fun A (bool)) s)) (v (fun A (bool)) s))) (v (fun A (bool)) s))))",
    Parse_tactic.parse "MP_TAC THM 2964929644107279817" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 2964929644107279817 ; THM 2527016271037755868 ]") true;;

(* "|- (!s. (:A) INTER s = s) /\ (!s. s INTER (:A) = s)" *)

register_proof 1585163697436276946 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) =) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (fun A (bool)))) INTER) (c (fun A (bool)) UNIV)) (v (fun A (bool)) s))) (v (fun A (bool)) s))))) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) =) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (fun A (bool)))) INTER) (v (fun A (bool)) s)) (c (fun A (bool)) UNIV))) (v (fun A (bool)) s)))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 2964929644107279817 ; THM 3311463869563081269 ; THM 2527016271037755868 ; THM 4053285993649185923 ]") true;;

(* "|- !s t. DISJOINT s t <=> DISJOINT t s" *)

register_proof 2768589221229041126 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) t) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) DISJOINT) (v (fun A (bool)) s)) (v (fun A (bool)) t))) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) DISJOINT) (v (fun A (bool)) t)) (v (fun A (bool)) s)))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 387987708984313766 ]") true;;

(* "|- !s. {} DIFF s = {}" *)

register_proof 1003480387060709729 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) =) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (fun A (bool)))) DIFF) (c (fun A (bool)) EMPTY)) (v (fun A (bool)) s))) (c (fun A (bool)) EMPTY))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 2527016271037755868 ; THM 454684675934084697 ; THM 1421231771456566753 ]") true;;

(* "|- !s. (:A) DIFF ((:A) DIFF s) = s" *)

register_proof 321505805930036595 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) =) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (fun A (bool)))) DIFF) (c (fun A (bool)) UNIV)) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (fun A (bool)))) DIFF) (c (fun A (bool)) UNIV)) (v (fun A (bool)) s)))) (v (fun A (bool)) s))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 2527016271037755868 ; THM 1421231771456566753 ; THM 3311463869563081269 ]") true;;

(* "|- !x s. x INSERT x INSERT s = x INSERT s" *)

register_proof 217501273267244146 (
  fun () ->
    decode_goal [] "(a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) =) (a (a (c (fun A (fun (fun A (bool)) (fun A (bool)))) INSERT) (v A x)) (a (a (c (fun A (fun (fun A (bool)) (fun A (bool)))) INSERT) (v A x)) (v (fun A (bool)) s)))) (a (a (c (fun A (fun (fun A (bool)) (fun A (bool)))) INSERT) (v A x)) (v (fun A (bool)) s)))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 463448412851097228 ; THM 3504165818671536592 ]") true;;

(* "|- !x s t.    x INSERT s UNION t =    (if x IN t then s UNION t else x INSERT (s UNION t))" *)

register_proof 2315713439052685376 (
  fun () ->
    decode_goal [] "(a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) t) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) =) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (fun A (bool)))) UNION) (a (a (c (fun A (fun (fun A (bool)) (fun A (bool)))) INSERT) (v A x)) (v (fun A (bool)) s))) (v (fun A (bool)) t))) (a (a (a (c (fun (bool) (fun (fun A (bool)) (fun (fun A (bool)) (fun A (bool))))) COND) (a (a (c (fun A (fun (fun A (bool)) (bool))) IN) (v A x)) (v (fun A (bool)) t))) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (fun A (bool)))) UNION) (v (fun A (bool)) s)) (v (fun A (bool)) t))) (a (a (c (fun A (fun (fun A (bool)) (fun A (bool)))) INSERT) (v A x)) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (fun A (bool)))) UNION) (v (fun A (bool)) s)) (v (fun A (bool)) t)))))))))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 975856962495483397 ; THM 1519551676238234042 ; THM 2527016271037755868 ; THM 463448412851097228 ; THM 2525030685148683581 ; THM 1564373864581257982 ; THM 1471035232732617433 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !x s. ~(x IN s) ==> (!t. s SUBSET x INSERT t <=> s SUBSET t)" *)

register_proof 1239327088731168250 (
  fun () ->
    decode_goal [] "(a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (c (fun (bool) (bool)) ~) (a (a (c (fun A (fun (fun A (bool)) (bool))) IN) (v A x)) (v (fun A (bool)) s)))) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) t) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) SUBSET) (v (fun A (bool)) s)) (a (a (c (fun A (fun (fun A (bool)) (fun A (bool)))) INSERT) (v A x)) (v (fun A (bool)) t)))) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) SUBSET) (v (fun A (bool)) s)) (v (fun A (bool)) t))))))))))",
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 1519551676238234042 ]" THEN
    Parse_tactic.parse "MP_TAC THM 3600758243849386141" THEN
    Parse_tactic.parse "SIMP_TAC [ ]" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 1041278397167009944 ; THM 463448412851097228 ; THM 4432927027650125967 ; THM 2622153333683456791 ; THM 1433978518111830493 ; THM 975856962495483397 ; THM 2527016271037755868 ; THM 2525030685148683581 ; THM 96506845967749275 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !x s. ~(x IN s) <=> s DELETE x = s" *)

register_proof 746831728234905130 (
  fun () ->
    decode_goal [] "(a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (bool) (bool)) ~) (a (a (c (fun A (fun (fun A (bool)) (bool))) IN) (v A x)) (v (fun A (bool)) s)))) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) =) (a (a (c (fun (fun A (bool)) (fun A (fun A (bool)))) DELETE) (v (fun A (bool)) s)) (v A x))) (v (fun A (bool)) s)))))))",
    Parse_tactic.parse "MP_TAC THM 1617287271715630323" THEN
    Parse_tactic.parse "SIMP_TAC [ THM 1041278397167009944 ; THM 1617287271715630323 ; THM 1433978518111830493 ; THM 96506845967749275 ; THM 2525030685148683581 ; THM 2315034643559247104 ; THM 3158068480668451995 ; THM 1409998405573405081 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1617287271715630323 ; THM 1433978518111830493 ]") true;;

(* "|- !x s. s DELETE x SUBSET s" *)

register_proof 1941804879973423645 (
  fun () ->
    decode_goal [] "(a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) SUBSET) (a (a (c (fun (fun A (bool)) (fun A (fun A (bool)))) DELETE) (v (fun A (bool)) s)) (v A x))) (v (fun A (bool)) s))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1617287271715630323 ; THM 3600758243849386141 ]") true;;

(* "|- !s t. s PSUBSET t <=> s SUBSET t /\ (?y. y IN t /\ ~(y IN s))" *)

register_proof 766676784989262785 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) t) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) PSUBSET) (v (fun A (bool)) s)) (v (fun A (bool)) t))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) SUBSET) (v (fun A (bool)) s)) (v (fun A (bool)) t))) (a (c (fun (fun A (bool)) (bool)) ?) (l (v A y) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun A (fun (fun A (bool)) (bool))) IN) (v A y)) (v (fun A (bool)) t))) (a (c (fun (bool) (bool)) ~) (a (a (c (fun A (fun (fun A (bool)) (bool))) IN) (v A y)) (v (fun A (bool)) s))))))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1606193632129706900 ]") true;;

(* "|- UNIONS {} = {}" *)

register_proof 1137231526941558137 (
  fun () ->
    decode_goal [] "(a (a (c (fun (fun ?56892 (bool)) (fun (fun ?56892 (bool)) (bool))) =) (a (c (fun (fun (fun ?56892 (bool)) (bool)) (fun ?56892 (bool))) UNIONS) (c (fun (fun ?56892 (bool)) (bool)) EMPTY))) (c (fun ?56892 (bool)) EMPTY))",
    Parse_tactic.parse "REWRITE_TAC [ THM 2527016271037755868 ; THM 4495262615029895189 ; THM 1433978518111830493 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 17050148103884248 ; THM 4495262615029895189 ; THM 1433978518111830493 ]") true;;

(* "|- !P s. (?x. x IN UNIONS s /\ P x) <=> (?t x. t IN s /\ x IN t /\ P x)" *)

register_proof 2066523243248307825 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun ?57016 (bool)) (bool)) (bool)) !) (l (v (fun ?57016 (bool)) P) (a (c (fun (fun (fun (fun ?57016 (bool)) (bool)) (bool)) (bool)) !) (l (v (fun (fun ?57016 (bool)) (bool)) s) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun ?57016 (bool)) (bool)) ?) (l (v ?57016 x) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun ?57016 (fun (fun ?57016 (bool)) (bool))) IN) (v ?57016 x)) (a (c (fun (fun (fun ?57016 (bool)) (bool)) (fun ?57016 (bool))) UNIONS) (v (fun (fun ?57016 (bool)) (bool)) s)))) (a (v (fun ?57016 (bool)) P) (v ?57016 x)))))) (a (c (fun (fun (fun ?57016 (bool)) (bool)) (bool)) ?) (l (v (fun ?57016 (bool)) t) (a (c (fun (fun ?57016 (bool)) (bool)) ?) (l (v ?57016 x) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (fun ?57016 (bool)) (fun (fun (fun ?57016 (bool)) (bool)) (bool))) IN) (v (fun ?57016 (bool)) t)) (v (fun (fun ?57016 (bool)) (bool)) s))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun ?57016 (fun (fun ?57016 (bool)) (bool))) IN) (v ?57016 x)) (v (fun ?57016 (bool)) t))) (a (v (fun ?57016 (bool)) P) (v ?57016 x)))))))))))))",
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 4316752623609269416 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 17050148103884248 ]") true;;

(* "|- !s. UNIONS ({} INSERT s) = UNIONS s" *)

register_proof 1830886683347601199 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun (fun ?57301 (bool)) (bool)) (bool)) (bool)) !) (l (v (fun (fun ?57301 (bool)) (bool)) s) (a (a (c (fun (fun ?57301 (bool)) (fun (fun ?57301 (bool)) (bool))) =) (a (c (fun (fun (fun ?57301 (bool)) (bool)) (fun ?57301 (bool))) UNIONS) (a (a (c (fun (fun ?57301 (bool)) (fun (fun (fun ?57301 (bool)) (bool)) (fun (fun ?57301 (bool)) (bool)))) INSERT) (c (fun ?57301 (bool)) EMPTY)) (v (fun (fun ?57301 (bool)) (bool)) s)))) (a (c (fun (fun (fun ?57301 (bool)) (bool)) (fun ?57301 (bool))) UNIONS) (v (fun (fun ?57301 (bool)) (bool)) s)))))",
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 625367087922838820 ]" THEN
    Parse_tactic.parse "MP_TAC THM 4423372279260857483" THEN
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 2270161700717384724 ]" THEN
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 1122139855026223720 ]" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 4423372279260857483 ; THM 2734571354838088458 ; THM 2332537566250341758 ; THM 2527016271037755868 ; THM 2356583009657867400 ; THM 2012640948690278230 ; THM 773808888572907911 ; THM 2076440368235384317 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- IMAGE f {} = {} /\ IMAGE f (x INSERT s) = f x INSERT IMAGE f s" *)

register_proof 2988388054771304757 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun (fun ?57512 (bool)) (fun (fun ?57512 (bool)) (bool))) =) (a (a (c (fun (fun ?57508 ?57512) (fun (fun ?57508 (bool)) (fun ?57512 (bool)))) IMAGE) (v (fun ?57508 ?57512) f)) (c (fun ?57508 (bool)) EMPTY))) (c (fun ?57512 (bool)) EMPTY))) (a (a (c (fun (fun ?57512 (bool)) (fun (fun ?57512 (bool)) (bool))) =) (a (a (c (fun (fun ?57508 ?57512) (fun (fun ?57508 (bool)) (fun ?57512 (bool)))) IMAGE) (v (fun ?57508 ?57512) f)) (a (a (c (fun ?57508 (fun (fun ?57508 (bool)) (fun ?57508 (bool)))) INSERT) (v ?57508 x)) (v (fun ?57508 (bool)) s)))) (a (a (c (fun ?57512 (fun (fun ?57512 (bool)) (fun ?57512 (bool)))) INSERT) (a (v (fun ?57508 ?57512) f) (v ?57508 x))) (a (a (c (fun (fun ?57508 ?57512) (fun (fun ?57508 (bool)) (fun ?57512 (bool)))) IMAGE) (v (fun ?57508 ?57512) f)) (v (fun ?57508 (bool)) s)))))",
    Parse_tactic.parse "REWRITE_TAC [ THM 2503682330082558798 ; THM 4495262615029895189 ; THM 1564373864581257982 ; THM 2527016271037755868 ; THM 463448412851097228 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !f s t. s SUBSET t ==> IMAGE f s SUBSET IMAGE f t" *)

register_proof 2975077475134135395 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun ?57624 ?57635) (bool)) (bool)) !) (l (v (fun ?57624 ?57635) f) (a (c (fun (fun (fun ?57624 (bool)) (bool)) (bool)) !) (l (v (fun ?57624 (bool)) s) (a (c (fun (fun (fun ?57624 (bool)) (bool)) (bool)) !) (l (v (fun ?57624 (bool)) t) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (fun ?57624 (bool)) (fun (fun ?57624 (bool)) (bool))) SUBSET) (v (fun ?57624 (bool)) s)) (v (fun ?57624 (bool)) t))) (a (a (c (fun (fun ?57635 (bool)) (fun (fun ?57635 (bool)) (bool))) SUBSET) (a (a (c (fun (fun ?57624 ?57635) (fun (fun ?57624 (bool)) (fun ?57635 (bool)))) IMAGE) (v (fun ?57624 ?57635) f)) (v (fun ?57624 (bool)) s))) (a (a (c (fun (fun ?57624 ?57635) (fun (fun ?57624 (bool)) (fun ?57635 (bool)))) IMAGE) (v (fun ?57624 ?57635) f)) (v (fun ?57624 (bool)) t))))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3600758243849386141 ; THM 2503682330082558798 ]") true;;

(* "|- !f s a.    (!x y. x IN s /\ y IN s /\ f x = f y ==> x = y) /\ a IN s    ==> IMAGE f (s DELETE a) = IMAGE f s DELETE f a" *)

register_proof 527036750737370453 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A B) (bool)) (bool)) !) (l (v (fun A B) f) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (c (fun (fun A (bool)) (bool)) !) (l (v A a) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (c (fun (fun A (bool)) (bool)) !) (l (v A x) (a (c (fun (fun A (bool)) (bool)) !) (l (v A y) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun A (fun (fun A (bool)) (bool))) IN) (v A x)) (v (fun A (bool)) s))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (a (c (fun A (fun (fun A (bool)) (bool))) IN) (v A y)) (v (fun A (bool)) s))) (a (a (c (fun B (fun B (bool))) =) (a (v (fun A B) f) (v A x))) (a (v (fun A B) f) (v A y)))))) (a (a (c (fun A (fun A (bool))) =) (v A x)) (v A y)))))))) (a (a (c (fun A (fun (fun A (bool)) (bool))) IN) (v A a)) (v (fun A (bool)) s)))) (a (a (c (fun (fun B (bool)) (fun (fun B (bool)) (bool))) =) (a (a (c (fun (fun A B) (fun (fun A (bool)) (fun B (bool)))) IMAGE) (v (fun A B) f)) (a (a (c (fun (fun A (bool)) (fun A (fun A (bool)))) DELETE) (v (fun A (bool)) s)) (v A a)))) (a (a (c (fun (fun B (bool)) (fun B (fun B (bool)))) DELETE) (a (a (c (fun (fun A B) (fun (fun A (bool)) (fun B (bool)))) IMAGE) (v (fun A B) f)) (v (fun A (bool)) s))) (a (v (fun A B) f) (v A a)))))))))))",
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 1519551676238234042 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3221052410856485961 ]") true;;

(* "|- !s c. IMAGE (\x. c) s = (if s = {} then {} else {c})" *)

register_proof 3675197758843132124 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun ?58126 (bool)) (bool)) (bool)) !) (l (v (fun ?58126 (bool)) s) (a (c (fun (fun ?58131 (bool)) (bool)) !) (l (v ?58131 c) (a (a (c (fun (fun ?58131 (bool)) (fun (fun ?58131 (bool)) (bool))) =) (a (a (c (fun (fun ?58126 ?58131) (fun (fun ?58126 (bool)) (fun ?58131 (bool)))) IMAGE) (l (v ?58126 x) (v ?58131 c))) (v (fun ?58126 (bool)) s))) (a (a (a (c (fun (bool) (fun (fun ?58131 (bool)) (fun (fun ?58131 (bool)) (fun ?58131 (bool))))) COND) (a (a (c (fun (fun ?58126 (bool)) (fun (fun ?58126 (bool)) (bool))) =) (v (fun ?58126 (bool)) s)) (c (fun ?58126 (bool)) EMPTY))) (c (fun ?58131 (bool)) EMPTY)) (a (a (c (fun ?58131 (fun (fun ?58131 (bool)) (fun ?58131 (bool)))) INSERT) (v ?58131 c)) (c (fun ?58131 (bool)) EMPTY))))))))",
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 1564373864581257982 ]" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 4495262615029895189 ; THM 2503682330082558798 ; THM 1659812372288323646 ; THM 2527016271037755868 ; THM 1564373864581257982 ; THM 3689011461107987624 ; THM 421915263640211727 ; THM 4316752623609269416 ; THM 698318653702143182 ]" THEN
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 1519551676238234042 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1433978518111830493 ; THM 463448412851097228 ]") true;;

(* "|- !s t.    (!y. y IN t ==> (?x. f x = y)) /\ (!x. f x IN t <=> x IN s)    ==> IMAGE f s = t" *)

register_proof 2466581896403718197 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun ?58301 (bool)) (bool)) (bool)) !) (l (v (fun ?58301 (bool)) s) (a (c (fun (fun (fun ?58297 (bool)) (bool)) (bool)) !) (l (v (fun ?58297 (bool)) t) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (c (fun (fun ?58297 (bool)) (bool)) !) (l (v ?58297 y) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun ?58297 (fun (fun ?58297 (bool)) (bool))) IN) (v ?58297 y)) (v (fun ?58297 (bool)) t))) (a (c (fun (fun ?58301 (bool)) (bool)) ?) (l (v ?58301 x) (a (a (c (fun ?58297 (fun ?58297 (bool))) =) (a (v (fun ?58301 ?58297) f) (v ?58301 x))) (v ?58297 y)))))))) (a (c (fun (fun ?58301 (bool)) (bool)) !) (l (v ?58301 x) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun ?58297 (fun (fun ?58297 (bool)) (bool))) IN) (a (v (fun ?58301 ?58297) f) (v ?58301 x))) (v (fun ?58297 (bool)) t))) (a (a (c (fun ?58301 (fun (fun ?58301 (bool)) (bool))) IN) (v ?58301 x)) (v (fun ?58301 (bool)) s))))))) (a (a (c (fun (fun ?58297 (bool)) (fun (fun ?58297 (bool)) (bool))) =) (a (a (c (fun (fun ?58301 ?58297) (fun (fun ?58301 (bool)) (fun ?58297 (bool)))) IMAGE) (v (fun ?58301 ?58297) f)) (v (fun ?58301 (bool)) s))) (v (fun ?58297 (bool)) t)))))))",
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 1519551676238234042 ]" THEN
    Parse_tactic.parse "MP_TAC THM 1519551676238234042" THEN
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 1041278397167009944 ]" THEN
    Parse_tactic.parse "MP_TAC THM 1124271433710135282" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 2503682330082558798 ; THM 1433978518111830493 ; THM 2527016271037755868 ; THM 4316752623609269416 ]") true;;

(* "|- !P. {p | P p} = {a,b | P (a,b)}" *)

register_proof 1109865616964734438 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun (prod ?58488 ?58487) (bool)) (bool)) (bool)) !) (l (v (fun (prod ?58488 ?58487) (bool)) P) (a (a (c (fun (fun (prod ?58488 ?58487) (bool)) (fun (fun (prod ?58488 ?58487) (bool)) (bool))) =) (a (c (fun (fun (prod ?58488 ?58487) (bool)) (fun (prod ?58488 ?58487) (bool))) GSPEC) (l (v (prod ?58488 ?58487) GEN%PVAR%19) (a (c (fun (fun (prod ?58488 ?58487) (bool)) (bool)) ?) (l (v (prod ?58488 ?58487) p) (a (a (a (c (fun (prod ?58488 ?58487) (fun (bool) (fun (prod ?58488 ?58487) (bool)))) SETSPEC) (v (prod ?58488 ?58487) GEN%PVAR%19)) (a (v (fun (prod ?58488 ?58487) (bool)) P) (v (prod ?58488 ?58487) p))) (v (prod ?58488 ?58487) p))))))) (a (c (fun (fun (prod ?58488 ?58487) (bool)) (fun (prod ?58488 ?58487) (bool))) GSPEC) (l (v (prod ?58488 ?58487) GEN%PVAR%20) (a (c (fun (fun ?58488 (bool)) (bool)) ?) (l (v ?58488 a) (a (c (fun (fun ?58487 (bool)) (bool)) ?) (l (v ?58487 b) (a (a (a (c (fun (prod ?58488 ?58487) (fun (bool) (fun (prod ?58488 ?58487) (bool)))) SETSPEC) (v (prod ?58488 ?58487) GEN%PVAR%20)) (a (v (fun (prod ?58488 ?58487) (bool)) P) (a (a (c (fun ?58488 (fun ?58487 (prod ?58488 ?58487))) ,) (v ?58488 a)) (v ?58487 b)))) (a (a (c (fun ?58488 (fun ?58487 (prod ?58488 ?58487))) ,) (v ?58488 a)) (v ?58487 b))))))))))))",
    Parse_tactic.parse "MP_TAC THM 280437576696359424" THEN
    Parse_tactic.parse "SUBST1_TAC THM 280437576696359424" THEN
    Parse_tactic.parse "SIMP_TAC [ THM 2525030685148683581 ; THM 96506845967749275 ; THM 4470124965642781870 ; THM 2527016271037755868 ; THM 698318653702143182 ]" THEN
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 4316752623609269416 ]" THEN
    Parse_tactic.parse "REWRITE_TAC [ THM 1376343245201252232 ; THM 358495441630787126 ; THM 3308271424913223125 ]") true;;

(* "|- !f s t. IMAGE f (s INTER t) SUBSET IMAGE f s INTER IMAGE f t" *)

register_proof 4063724346477153344 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun ?60097 ?60108) (bool)) (bool)) !) (l (v (fun ?60097 ?60108) f) (a (c (fun (fun (fun ?60097 (bool)) (bool)) (bool)) !) (l (v (fun ?60097 (bool)) s) (a (c (fun (fun (fun ?60097 (bool)) (bool)) (bool)) !) (l (v (fun ?60097 (bool)) t) (a (a (c (fun (fun ?60108 (bool)) (fun (fun ?60108 (bool)) (bool))) SUBSET) (a (a (c (fun (fun ?60097 ?60108) (fun (fun ?60097 (bool)) (fun ?60108 (bool)))) IMAGE) (v (fun ?60097 ?60108) f)) (a (a (c (fun (fun ?60097 (bool)) (fun (fun ?60097 (bool)) (fun ?60097 (bool)))) INTER) (v (fun ?60097 (bool)) s)) (v (fun ?60097 (bool)) t)))) (a (a (c (fun (fun ?60108 (bool)) (fun (fun ?60108 (bool)) (fun ?60108 (bool)))) INTER) (a (a (c (fun (fun ?60097 ?60108) (fun (fun ?60097 (bool)) (fun ?60108 (bool)))) IMAGE) (v (fun ?60097 ?60108) f)) (v (fun ?60097 (bool)) s))) (a (a (c (fun (fun ?60097 ?60108) (fun (fun ?60097 (bool)) (fun ?60108 (bool)))) IMAGE) (v (fun ?60097 ?60108) f)) (v (fun ?60097 (bool)) t))))))))))",
    Parse_tactic.parse "SIMP_TAC [ THM 952420646866288674 ; THM 4038764519883693792 ; THM 2975077475134135395 ]") true;;

(* "|- ~(s IN g) ==> g DELETE s = g" *)

register_proof 3174580013930275692 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (c (fun (bool) (bool)) ~) (a (a (c (fun ?60366 (fun (fun ?60366 (bool)) (bool))) IN) (v ?60366 s)) (v (fun ?60366 (bool)) g)))) (a (a (c (fun (fun ?60366 (bool)) (fun (fun ?60366 (bool)) (bool))) =) (a (a (c (fun (fun ?60366 (bool)) (fun ?60366 (fun ?60366 (bool)))) DELETE) (v (fun ?60366 (bool)) g)) (v ?60366 s))) (v (fun ?60366 (bool)) g)))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 746831728234905130 ]") true;;

(* "|- !P f.    (!x y. P x /\ P y /\ f x = f y ==> x = y) <=>    (!x y. P x /\ P y ==> (f x = f y <=> x = y))" *)

register_proof 3727644849293507323 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun ?60768 (bool)) (bool)) (bool)) !) (l (v (fun ?60768 (bool)) P) (a (c (fun (fun (fun ?60768 ?60763) (bool)) (bool)) !) (l (v (fun ?60768 ?60763) f) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun ?60768 (bool)) (bool)) !) (l (v ?60768 x) (a (c (fun (fun ?60768 (bool)) (bool)) !) (l (v ?60768 y) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (v (fun ?60768 (bool)) P) (v ?60768 x))) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (v (fun ?60768 (bool)) P) (v ?60768 y))) (a (a (c (fun ?60763 (fun ?60763 (bool))) =) (a (v (fun ?60768 ?60763) f) (v ?60768 x))) (a (v (fun ?60768 ?60763) f) (v ?60768 y)))))) (a (a (c (fun ?60768 (fun ?60768 (bool))) =) (v ?60768 x)) (v ?60768 y)))))))) (a (c (fun (fun ?60768 (bool)) (bool)) !) (l (v ?60768 x) (a (c (fun (fun ?60768 (bool)) (bool)) !) (l (v ?60768 y) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (bool) (fun (bool) (bool))) /\) (a (v (fun ?60768 (bool)) P) (v ?60768 x))) (a (v (fun ?60768 (bool)) P) (v ?60768 y)))) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (a (c (fun ?60763 (fun ?60763 (bool))) =) (a (v (fun ?60768 ?60763) f) (v ?60768 x))) (a (v (fun ?60768 ?60763) f) (v ?60768 y)))) (a (a (c (fun ?60768 (fun ?60768 (bool))) =) (v ?60768 x)) (v ?60768 y)))))))))))))",
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;

(* "|- !f. (!y. ?x. f x = y) <=> (!P. (?x. P (f x)) <=> (?y. P y))" *)

register_proof 3127606599017011005 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A B) (bool)) (bool)) !) (l (v (fun A B) f) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun B (bool)) (bool)) !) (l (v B y) (a (c (fun (fun A (bool)) (bool)) ?) (l (v A x) (a (a (c (fun B (fun B (bool))) =) (a (v (fun A B) f) (v A x))) (v B y))))))) (a (c (fun (fun (fun B (bool)) (bool)) (bool)) !) (l (v (fun B (bool)) P) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun A (bool)) (bool)) ?) (l (v A x) (a (v (fun B (bool)) P) (a (v (fun A B) f) (v A x)))))) (a (c (fun (fun B (bool)) (bool)) ?) (l (v B y) (a (v (fun B (bool)) P) (v B y))))))))))",
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 3713350661807540262 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 2503682330082558798 ; THM 1433978518111830493 ; THM 4495262615029895189 ; THM 463448412851097228 ]") true;;

(* "|- !a. FINITE {a}" *)

register_proof 3362165900392862375 (
  fun () ->
    decode_goal [] "(a (c (fun (fun ?61935 (bool)) (bool)) !) (l (v ?61935 a) (a (c (fun (fun ?61935 (bool)) (bool)) FINITE) (a (a (c (fun ?61935 (fun (fun ?61935 (bool)) (fun ?61935 (bool)))) INSERT) (v ?61935 a)) (c (fun ?61935 (bool)) EMPTY)))))",
    Parse_tactic.parse "ASM_MESON_TAC [ THM 3641629547051222928 ]") true;;

(* "|- !f s. FINITE s ==> FINITE (IMAGE f s)" *)

register_proof 3958922733320909097 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun A B) (bool)) (bool)) !) (l (v (fun A B) f) (a (c (fun (fun (fun A (bool)) (bool)) (bool)) !) (l (v (fun A (bool)) s) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (c (fun (fun A (bool)) (bool)) FINITE) (v (fun A (bool)) s))) (a (c (fun (fun B (bool)) (bool)) FINITE) (a (a (c (fun (fun A B) (fun (fun A (bool)) (fun B (bool)))) IMAGE) (v (fun A B) f)) (v (fun A (bool)) s))))))))",
    Parse_tactic.parse "ONCE_REWRITE_TAC [ THM 2564420325514287403 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 1888179071220440778 ]") true;;

(* "|- !P f s.    (!t. t SUBSET IMAGE f s ==> P t) <=>    (!t. t SUBSET s ==> P (IMAGE f t))" *)

register_proof 4106943071244794807 (
  fun () ->
    decode_goal [] "(a (c (fun (fun (fun (fun ?62901 (bool)) (bool)) (bool)) (bool)) !) (l (v (fun (fun ?62901 (bool)) (bool)) P) (a (c (fun (fun (fun ?62885 ?62901) (bool)) (bool)) !) (l (v (fun ?62885 ?62901) f) (a (c (fun (fun (fun ?62885 (bool)) (bool)) (bool)) !) (l (v (fun ?62885 (bool)) s) (a (a (c (fun (bool) (fun (bool) (bool))) =) (a (c (fun (fun (fun ?62901 (bool)) (bool)) (bool)) !) (l (v (fun ?62901 (bool)) t) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (fun ?62901 (bool)) (fun (fun ?62901 (bool)) (bool))) SUBSET) (v (fun ?62901 (bool)) t)) (a (a (c (fun (fun ?62885 ?62901) (fun (fun ?62885 (bool)) (fun ?62901 (bool)))) IMAGE) (v (fun ?62885 ?62901) f)) (v (fun ?62885 (bool)) s)))) (a (v (fun (fun ?62901 (bool)) (bool)) P) (v (fun ?62901 (bool)) t)))))) (a (c (fun (fun (fun ?62885 (bool)) (bool)) (bool)) !) (l (v (fun ?62885 (bool)) t) (a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (a (c (fun (fun ?62885 (bool)) (fun (fun ?62885 (bool)) (bool))) SUBSET) (v (fun ?62885 (bool)) t)) (v (fun ?62885 (bool)) s))) (a (v (fun (fun ?62901 (bool)) (bool)) P) (a (a (c (fun (fun ?62885 ?62901) (fun (fun ?62885 (bool)) (fun ?62901 (bool)))) IMAGE) (v (fun ?62885 ?62901) f)) (v (fun ?62885 (bool)) t)))))))))))))",
    Parse_tactic.parse "MP_TAC THM 3600758243849386141" THEN
    Parse_tactic.parse "MP_TAC THM 1041278397167009944" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ THM 4293449629756068693 ]") true;;

(* "|- ~(x IN s) ==> (x INSERT s) DELETE x = s" *)

register_proof 2594494518572862670 (
  fun () ->
    decode_goal [] "(a (a (c (fun (bool) (fun (bool) (bool))) ==>) (a (c (fun (bool) (bool)) ~) (a (a (c (fun A (fun (fun A (bool)) (bool))) IN) (v A x)) (v (fun A (bool)) s)))) (a (a (c (fun (fun A (bool)) (fun (fun A (bool)) (bool))) =) (a (a (c (fun (fun A (bool)) (fun A (fun A (bool)))) DELETE) (a (a (c (fun A (fun (fun A (bool)) (fun A (bool)))) INSERT) (v A x)) (v (fun A (bool)) s))) (v A x))) (v (fun A (bool)) s)))",
    Parse_tactic.parse "REWRITE_TAC [ THM 2527016271037755868 ; THM 1617287271715630323 ; THM 463448412851097228 ; THM 3504417690786988252 ; THM 1433978518111830493 ]" THEN
    Parse_tactic.parse "ASM_MESON_TAC [ ]") true;;
