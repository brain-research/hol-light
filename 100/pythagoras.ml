(* ========================================================================= *)
(* A "proof" of Pythagoras's theorem. Of course something similar is         *)
(* implicit in the definition of "norm", but maybe this is still nontrivial. *)
(* ========================================================================= *)

set_jrh_lexer;;
Pb_printer.set_file_tags ["Top100"; "pythagoras.ml"];;

open Parser;;
open Tactics;;
open Simp;;
open Calc_num;;
open Calc_rat;;
open Cart;;

open Vectors;;

(* ------------------------------------------------------------------------- *)
(* Direct vector proof (could replace 2 by N and the proof still runs).      *)
(* ------------------------------------------------------------------------- *)

let PYTHAGORAS = prove
 (`!A B C:real^2.
        orthogonal (A - B) (C - B)
        ==> norm(C - A) pow 2 = norm(B - A) pow 2 + norm(C - B) pow 2`,
  REWRITE_TAC[NORM_POW_2; orthogonal; DOT_LSUB; DOT_RSUB; DOT_SYM] THEN
  CONV_TAC "100/pythagoras.ml:REAL_RING" REAL_RING);;

(* ------------------------------------------------------------------------- *)
(* A more explicit and laborious "componentwise" specifically for 2-vectors. *)
(* ------------------------------------------------------------------------- *)

let PYTHAGORAS = prove
 (`!A B C:real^2.
        orthogonal (A - B) (C - B)
        ==> norm(C - A) pow 2 = norm(B - A) pow 2 + norm(C - B) pow 2`,
  SIMP_TAC[NORM_POW_2; orthogonal; dot; SUM_2; DIMINDEX_2;
           VECTOR_SUB_COMPONENT; ARITH] THEN
  CONV_TAC "100/pythagoras.ml:REAL_RING" REAL_RING);;
Pb_printer.clear_file_tags();;
