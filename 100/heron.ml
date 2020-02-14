(* ========================================================================= *)
(* Heron's formula for the area of a triangle.                               *)
(* ========================================================================= *)

set_jrh_lexer;;
Pb_printer.set_file_tags ["Top100"; "heron.ml"];;

open Lib;;
open Fusion;;
open Basics;;
open Parser;;
open Equal;;
open Bool;;
open Tactics;;
open Simp;;
open Pair;;
open Calc_num;;
open Realax;;
open Realarith;;
open Reals;;
open Calc_rat;;
open Cart;;
open Misc;;

open Vectors;;
open Measure;;

prioritize_real();;

(* ------------------------------------------------------------------------- *)
(* Eliminate square roots from formula by the usual method.                  *)
(* ------------------------------------------------------------------------- *)

let SQRT_ELIM_TAC =
  let sqrt_tm = `sqrt:real->real` in
  let is_sqrt tm = is_comb tm && rator tm = sqrt_tm in
  fun (asl,w) ->
    let stms = setify(find_terms is_sqrt w) in
    let gvs = map (genvar o type_of) stms in
    (MAP_EVERY (MP_TAC o C SPEC SQRT_POW_2 o rand) stms THEN
     EVERY (map2 (fun s v -> SPEC_TAC(s,v)) stms gvs)) (asl,w);;

(* ------------------------------------------------------------------------- *)
(* Main result.                                                              *)
(* ------------------------------------------------------------------------- *)

let HERON = prove
 (`!A B C:real^2. 
        let a = dist(C,B)
        and b = dist(A,C)
        and c = dist(B,A) in
        let s = (a + b + c) / &2 in
        measure(convex hull {A,B,C}) = sqrt(s * (s - a) * (s - b) * (s - c))`,
  REPEAT GEN_TAC THEN REWRITE_TAC[LET_DEF; LET_END_DEF] THEN
  REWRITE_TAC[MEASURE_TRIANGLE] THEN
  CONV_TAC "100/heron.ml:SYM_CONV" SYM_CONV THEN MATCH_MP_TAC SQRT_UNIQUE THEN
  SIMP_TAC[REAL_LE_DIV; REAL_ABS_POS; REAL_POS] THEN
  REWRITE_TAC[REAL_POW_DIV; REAL_POW2_ABS] THEN
  REWRITE_TAC[dist; vector_norm] THEN
  REWRITE_TAC[dot; SUM_2; DIMINDEX_2] THEN
  SIMP_TAC[VECTOR_SUB_COMPONENT; ARITH; DIMINDEX_2] THEN
  SQRT_ELIM_TAC THEN SIMP_TAC[REAL_LE_SQUARE; REAL_LE_ADD] THEN
  CONV_TAC "100/heron.ml:REAL_RING" REAL_RING);;
Pb_printer.clear_file_tags();;
