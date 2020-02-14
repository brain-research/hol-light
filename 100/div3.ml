(* ========================================================================= *)
(* #85: divisibility by 3 rule                                               *)
(* ========================================================================= *)

set_jrh_lexer;;
Pb_printer.set_file_tags ["Top100"; "div3.ml"];;

open Parser;;
open Equal;;
open Tactics;;
open Simp;;
open Nums;;
open Arith;;
open Calc_num;;
open Ints;;
open Iterate;;

open Pocklington;;

let EXP_10_CONG_3 = prove
 (`!n. (10 EXP n == 1) (mod 3)`,
  INDUCT_TAC THEN REWRITE_TAC[EXP; CONG_REFL] THEN
  MATCH_MP_TAC CONG_TRANS THEN EXISTS_TAC `10 * 1` THEN CONJ_TAC THEN
  ASM_SIMP_TAC[CONG_MULT; CONG_REFL] THEN
  SIMP_TAC[CONG; ARITH] THEN CONV_TAC "100/div3.ml:NUM_REDUCE_CONV" NUM_REDUCE_CONV);;

let SUM_CONG_3 = prove
 (`!d n. (nsum(0..n) (\i. 10 EXP i * d(i)) == nsum(0..n) (\i. d i)) (mod 3)`,
  GEN_TAC THEN INDUCT_TAC THEN REWRITE_TAC[NSUM_CLAUSES_NUMSEG] THENL
   [REWRITE_TAC[EXP; MULT_CLAUSES; CONG_REFL]; ALL_TAC] THEN
  REWRITE_TAC[LE_0] THEN MATCH_MP_TAC CONG_ADD THEN
  ASM_REWRITE_TAC[] THEN
  GEN_REWRITE_TAC "100/div3.ml:(LAND_CONV)" (LAND_CONV) [ARITH_RULE `d = 1 * d`] THEN
  MATCH_MP_TAC CONG_MULT THEN REWRITE_TAC[CONG_REFL; EXP_10_CONG_3]);;

let DIVISIBILITY_BY_3 = prove
 (`3 divides (nsum(0..n) (\i. 10 EXP i * d(i))) <=>
   3 divides (nsum(0..n) (\i. d i))`,
  MATCH_MP_TAC CONG_DIVIDES THEN REWRITE_TAC[SUM_CONG_3]);;
Pb_printer.clear_file_tags();;
