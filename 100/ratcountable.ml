(* ========================================================================= *)
(* Theorem 3: countability of rational numbers.                              *)
(* ========================================================================= *)

set_jrh_lexer;;
Pb_printer.set_file_tags ["Top100"; "ratcountable.ml"];;

open Parser;;
open Equal;;
open Tactics;;
open Simp;;
open Theorems;;
open Meson;;
open Pair;;
open Calc_num;;
open Realax;;
open Calc_int;;
open Reals;;
open Calc_rat;;
open Sets;;
open Card;;

prioritize_real();;

(* ------------------------------------------------------------------------- *)
(* Definition of rational and countable.                                     *)
(* ------------------------------------------------------------------------- *)

let ratcountable_rational = new_definition
  `ratcountable_rational(r) <=> ?p q. ~(q = 0) /\ (abs(r) = &p / &q)`;;

let countable = new_definition
  `countable s <=> s <=_c (UNIV:num->bool)`;;

(* ------------------------------------------------------------------------- *)
(* Proof of the main result.                                                 *)
(* ------------------------------------------------------------------------- *)

let COUNTABLE_RATIONALS = prove
 (`countable { x:real | ratcountable_rational(x)}`,
  REWRITE_TAC[countable] THEN TRANS_TAC CARD_LE_TRANS
   `{ x:real | ?p q. x = &p / &q } *_c (UNIV:num->bool)` THEN
  CONJ_TAC THENL
   [REWRITE_TAC[LE_C; EXISTS_PAIR_THM; mul_c] THEN
    EXISTS_TAC `\(x,b). if b = 0 then x else --x` THEN
    CONV_TAC "100/ratcountable.ml:(ONCE_DEPTH_CONV GEN_BETA_CONV)" (ONCE_DEPTH_CONV GEN_BETA_CONV) THEN
    REWRITE_TAC[IN_ELIM_THM; ratcountable_rational; IN_UNIV; PAIR_EQ] THEN
    MESON_TAC[REAL_ARITH `(abs(x) = a) ==> (x = a) \/ x = --a`];
    ALL_TAC] THEN
  MATCH_MP_TAC CARD_MUL_ABSORB_LE THEN REWRITE_TAC[num_INFINITE] THEN
  TRANS_TAC CARD_LE_TRANS `(UNIV *_c UNIV):num#num->bool` THEN CONJ_TAC THENL
   [REWRITE_TAC[LE_C; EXISTS_PAIR_THM; mul_c; IN_UNIV] THEN
    EXISTS_TAC `\(p,q). &p / &q` THEN
    CONV_TAC "100/ratcountable.ml:(ONCE_DEPTH_CONV GEN_BETA_CONV)" (ONCE_DEPTH_CONV GEN_BETA_CONV) THEN
    REWRITE_TAC[IN_ELIM_THM; ratcountable_rational] THEN MESON_TAC[];
    MESON_TAC[CARD_MUL_ABSORB_LE; CARD_LE_REFL; num_INFINITE]]);;

(* ------------------------------------------------------------------------- *)
(* Maybe I should actually prove equality?                                   *)
(* ------------------------------------------------------------------------- *)

let denumerable = new_definition
  `denumerable s <=> s =_c (UNIV:num->bool)`;;

let DENUMERABLE_RATIONALS = prove
 (`denumerable { x:real | ratcountable_rational(x)}`,
  REWRITE_TAC[denumerable; GSYM CARD_LE_ANTISYM] THEN
  REWRITE_TAC[GSYM countable; COUNTABLE_RATIONALS] THEN
  REWRITE_TAC[le_c] THEN EXISTS_TAC `&` THEN
  SIMP_TAC[IN_ELIM_THM; IN_UNIV; REAL_OF_NUM_EQ; ratcountable_rational] THEN
  X_GEN_TAC `p:num` THEN MAP_EVERY EXISTS_TAC [`p:num`; `1`] THEN
  REWRITE_TAC[REAL_DIV_1; REAL_ABS_NUM; ARITH_EQ]);;

(* ------------------------------------------------------------------------- *)
(* Expand out the cardinal comparison definitions for explicitness.          *)
(* ------------------------------------------------------------------------- *)

let DENUMERABLE_RATIONALS_EXPAND = prove
 (`?rat:num->real. (!n. ratcountable_rational(rat n)) /\ 
                   (!x. ratcountable_rational x ==> ?!n. x = rat n)`,
  MP_TAC DENUMERABLE_RATIONALS THEN REWRITE_TAC[denumerable] THEN
  ONCE_REWRITE_TAC[CARD_EQ_SYM] THEN REWRITE_TAC[eq_c] THEN
  REWRITE_TAC[IN_UNIV; IN_ELIM_THM] THEN 
  MATCH_MP_TAC MONO_EXISTS THEN MESON_TAC[]);;
Pb_printer.clear_file_tags();;
