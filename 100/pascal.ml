(* ========================================================================= *)
(* Pascal's hexagon theorem for projective and affine planes.                *)
(* ========================================================================= *)

set_jrh_lexer;;
Pb_printer.set_file_tags ["Top100"; "pascal.ml"];;

open Lib;;
open Fusion;;
open Parser;;
open Equal;;
open Bool;;
open Drule;;
open Tactics;;
open Simp;;
open Theorems;;
open Class;;
open Trivia;;
open Meson;;
open Pair;;
open Lists;;
open Realax;;
open Calc_int;;
open Reals;;
open Calc_rat;;
open Sets;;
open Cart;;

open Vectors;;
open Determinants;;

open Cross;;
open Desargues;;

(* NOTE: Many duplicate theorems and definitions from desargues.ml are removed*)

(* ------------------------------------------------------------------------- *)
(* Conics and bracket condition for 6 points to be on a conic.               *)
(* ------------------------------------------------------------------------- *)

let homogeneous_conic = new_definition
 `homogeneous_conic con <=>
    ?a b c d e f.
       ~(a = &0 /\ b = &0 /\ c = &0 /\ d = &0 /\ e = &0 /\ f = &0) /\
       con = {x:real^3 | a * x$1 pow 2 + b * x$2 pow 2 + c * x$3 pow 2 +
                         d * x$1 * x$2 + e * x$1 * x$3 + f * x$2 * x$3 = &0}`;;

let projective_conic = new_definition
 `projective_conic con <=>
        ?c. homogeneous_conic c /\ con = {p | (homop p) IN c}`;;

let HOMOGENEOUS_CONIC_BRACKET = prove
 (`!con x1 x2 x3 x4 x5 x6.
        homogeneous_conic con /\
        x1 IN con /\ x2 IN con /\ x3 IN con /\
        x4 IN con /\ x5 IN con /\ x6 IN con
        ==> det(vector[x6;x1;x4]) * det(vector[x6;x2;x3]) *
            det(vector[x5;x1;x3]) * det(vector[x5;x2;x4]) =
            det(vector[x6;x1;x3]) * det(vector[x6;x2;x4]) *
            det(vector[x5;x1;x4]) * det(vector[x5;x2;x3])`,
  REPEAT GEN_TAC THEN REWRITE_TAC[homogeneous_conic; EXTENSION] THEN
  ONCE_REWRITE_TAC[IMP_CONJ] THEN REWRITE_TAC[LEFT_IMP_EXISTS_THM] THEN
  REPEAT GEN_TAC THEN DISCH_THEN(CONJUNCTS_THEN2 MP_TAC ASSUME_TAC) THEN
  ASM_REWRITE_TAC[IN_ELIM_THM; DET_3; VECTOR_3] THEN
  CONV_TAC "100/pascal.ml:REAL_RING" REAL_RING);;

let PROJECTIVE_CONIC_BRACKET = prove
 (`!con p1 p2 p3 p4 p5 p6.
        projective_conic con /\
        p1 IN con /\ p2 IN con /\ p3 IN con /\
        p4 IN con /\ p5 IN con /\ p6 IN con
        ==> bracket[p6;p1;p4] * bracket[p6;p2;p3] *
            bracket[p5;p1;p3] * bracket[p5;p2;p4] =
            bracket[p6;p1;p3] * bracket[p6;p2;p4] *
            bracket[p5;p1;p4] * bracket[p5;p2;p3]`,
  REPEAT GEN_TAC THEN REWRITE_TAC[bracket; projective_conic] THEN
  DISCH_THEN(CONJUNCTS_THEN2 STRIP_ASSUME_TAC MP_TAC) THEN
  ASM_REWRITE_TAC[IN_ELIM_THM] THEN STRIP_TAC THEN
  MATCH_MP_TAC HOMOGENEOUS_CONIC_BRACKET THEN ASM_MESON_TAC[]);;

(* ------------------------------------------------------------------------- *)
(* Pascal's theorem with all the nondegeneracy principles we use directly.   *)
(* ------------------------------------------------------------------------- *)

let PASCAL_DIRECT = prove
 (`!con x1 x2 x3 x4 x5 x6 x6 x8 x9.
        ~COLLINEAR {x2,x5,x7} /\
        ~COLLINEAR {x1,x2,x5} /\
        ~COLLINEAR {x1,x3,x6} /\
        ~COLLINEAR {x2,x4,x6} /\
        ~COLLINEAR {x3,x4,x5} /\
        ~COLLINEAR {x1,x5,x7} /\
        ~COLLINEAR {x2,x5,x9} /\
        ~COLLINEAR {x1,x2,x6} /\
        ~COLLINEAR {x3,x6,x8} /\
        ~COLLINEAR {x2,x4,x5} /\
        ~COLLINEAR {x2,x4,x7} /\
        ~COLLINEAR {x2,x6,x8} /\
        ~COLLINEAR {x3,x4,x6} /\
        ~COLLINEAR {x3,x5,x8} /\
        ~COLLINEAR {x1,x3,x5}
        ==> projective_conic con /\
            x1 IN con /\ x2 IN con /\ x3 IN con /\
            x4 IN con /\ x5 IN con /\ x6 IN con /\
            COLLINEAR {x1,x9,x5} /\
            COLLINEAR {x1,x8,x6} /\
            COLLINEAR {x2,x9,x4} /\
            COLLINEAR {x2,x7,x6} /\
            COLLINEAR {x3,x8,x4} /\
            COLLINEAR {x3,x7,x5}
            ==> COLLINEAR {x7,x8,x9}`,
  REPEAT GEN_TAC THEN DISCH_TAC THEN
  REWRITE_TAC[TAUT `a /\ b /\ c /\ d /\ e /\ f /\ g /\ h ==> p <=>
                    a /\ b /\ c /\ d /\ e /\ f /\ g ==> h ==> p`] THEN
  DISCH_THEN(MP_TAC o MATCH_MP PROJECTIVE_CONIC_BRACKET) THEN
  REWRITE_TAC[COLLINEAR_BRACKET; IMP_IMP; GSYM CONJ_ASSOC] THEN
  MATCH_MP_TAC(TAUT `!q. (p ==> q) /\ (q ==> r) ==> p ==> r`) THEN
  EXISTS_TAC
   `bracket[x1;x2;x5] * bracket[x1;x3;x6] *
    bracket[x2;x4;x6] * bracket[x3;x4;x5] =
    bracket[x1;x2;x6] * bracket[x1;x3;x5] *
    bracket[x2;x4;x5] * bracket[x3;x4;x6] /\
    bracket[x1;x5;x7] * bracket[x2;x5;x9] =
    --bracket[x1;x2;x5] * bracket[x5;x9;x7] /\
    bracket[x1;x2;x6] * bracket[x3;x6;x8] =
    bracket[x1;x3;x6] * bracket[x2;x6;x8] /\
    bracket[x2;x4;x5] * bracket[x2;x9;x7] =
    --bracket[x2;x4;x7] * bracket[x2;x5;x9] /\
    bracket[x2;x4;x7] * bracket[x2;x6;x8] =
    --bracket[x2;x4;x6] * bracket[x2;x8;x7] /\
    bracket[x3;x4;x6] * bracket[x3;x5;x8] =
    bracket[x3;x4;x5] * bracket[x3;x6;x8] /\
    bracket[x1;x3;x5] * bracket[x5;x8;x7] =
    --bracket[x1;x5;x7] * bracket[x3;x5;x8]` THEN
  CONJ_TAC THENL
   [REPEAT(MATCH_MP_TAC MONO_AND THEN CONJ_TAC) THEN
    REWRITE_TAC[bracket; DET_3; VECTOR_3] THEN CONV_TAC "100/pascal.ml:REAL_RING" REAL_RING;
    ALL_TAC] THEN
  REWRITE_TAC[IMP_CONJ] THEN
  REPEAT(ONCE_REWRITE_TAC[IMP_IMP] THEN
         DISCH_THEN(MP_TAC o MATCH_MP (REAL_RING
          `a = b /\ x:real = y ==> a * x = b * y`))) THEN
  REWRITE_TAC[GSYM REAL_MUL_ASSOC; REAL_MUL_LNEG; REAL_MUL_RNEG] THEN
  REWRITE_TAC[REAL_NEG_NEG] THEN
  RULE_ASSUM_TAC(REWRITE_RULE[COLLINEAR_BRACKET]) THEN
  REWRITE_TAC[REAL_MUL_AC] THEN ASM_REWRITE_TAC[REAL_EQ_MUL_LCANCEL] THEN
  ONCE_REWRITE_TAC[REAL_MUL_SYM] THEN REWRITE_TAC[GSYM REAL_MUL_ASSOC] THEN
  ASM_REWRITE_TAC[REAL_EQ_MUL_LCANCEL] THEN
  FIRST_X_ASSUM(MP_TAC o CONJUNCT1) THEN
  REWRITE_TAC[bracket; DET_3; VECTOR_3] THEN CONV_TAC "100/pascal.ml:REAL_RING" REAL_RING);;

(* ------------------------------------------------------------------------- *)
(* With longer but more intuitive non-degeneracy conditions, basically that  *)
(* the 6 points divide into two groups of 3 and no 3 are collinear unless    *)
(* they are all in the same group.                                           *)
(* ------------------------------------------------------------------------- *)

let PASCAL = prove
 (`!con x1 x2 x3 x4 x5 x6 x6 x8 x9.
        ~COLLINEAR {x1,x2,x4} /\
        ~COLLINEAR {x1,x2,x5} /\
        ~COLLINEAR {x1,x2,x6} /\
        ~COLLINEAR {x1,x3,x4} /\
        ~COLLINEAR {x1,x3,x5} /\
        ~COLLINEAR {x1,x3,x6} /\
        ~COLLINEAR {x2,x3,x4} /\
        ~COLLINEAR {x2,x3,x5} /\
        ~COLLINEAR {x2,x3,x6} /\
        ~COLLINEAR {x4,x5,x1} /\
        ~COLLINEAR {x4,x5,x2} /\
        ~COLLINEAR {x4,x5,x3} /\
        ~COLLINEAR {x4,x6,x1} /\
        ~COLLINEAR {x4,x6,x2} /\
        ~COLLINEAR {x4,x6,x3} /\
        ~COLLINEAR {x5,x6,x1} /\
        ~COLLINEAR {x5,x6,x2} /\
        ~COLLINEAR {x5,x6,x3}
        ==> projective_conic con /\
            x1 IN con /\ x2 IN con /\ x3 IN con /\
            x4 IN con /\ x5 IN con /\ x6 IN con /\
            COLLINEAR {x1,x9,x5} /\
            COLLINEAR {x1,x8,x6} /\
            COLLINEAR {x2,x9,x4} /\
            COLLINEAR {x2,x7,x6} /\
            COLLINEAR {x3,x8,x4} /\
            COLLINEAR {x3,x7,x5}
            ==> COLLINEAR {x7,x8,x9}`,
  REPEAT GEN_TAC THEN DISCH_TAC THEN
  DISCH_THEN(fun th ->
    MATCH_MP_TAC(TAUT `(~p ==> p) ==> p`) THEN DISCH_TAC THEN
    MP_TAC th THEN MATCH_MP_TAC PASCAL_DIRECT THEN
    ASSUME_TAC(funpow 7 CONJUNCT2 th)) THEN
  REPEAT CONJ_TAC THEN
  REPEAT(POP_ASSUM MP_TAC) THEN
  REWRITE_TAC[COLLINEAR_BRACKET; bracket; DET_3; VECTOR_3] THEN
  CONV_TAC "100/pascal.ml:REAL_RING" REAL_RING);;

(* ------------------------------------------------------------------------- *)
(* Homogenization and hence mapping from affine to projective plane.         *)
(* ------------------------------------------------------------------------- *)

let homogenize = new_definition
 `(homogenize:real^2->real^3) x = vector[x$1; x$2; &1]`;;

let projectivize = new_definition
 `projectivize = projp o homogenize`;;

let HOMOGENIZE_NONZERO = prove
 (`!x. ~(homogenize x = vec 0)`,
  REWRITE_TAC[CART_EQ; DIMINDEX_3; FORALL_3; VEC_COMPONENT; VECTOR_3;
              homogenize] THEN
  REAL_ARITH_TAC);;

(* ------------------------------------------------------------------------- *)
(* Conic in affine plane.                                                    *)
(* ------------------------------------------------------------------------- *)

let affine_conic = new_definition
 `affine_conic con <=>
    ?a b c d e f.
       ~(a = &0 /\ b = &0 /\ c = &0 /\ d = &0 /\ e = &0 /\ f = &0) /\
       con = {x:real^2 | a * x$1 pow 2 + b * x$2 pow 2 + c * x$1 * x$2 +
                         d * x$1 + e * x$2 + f = &0}`;;

(* ------------------------------------------------------------------------- *)
(* Relationships between affine and projective notions.                      *)
(* ------------------------------------------------------------------------- *)

let COLLINEAR_PROJECTIVIZE = prove
 (`!a b c. collinear{a,b,c} <=>
           COLLINEAR{projectivize a,projectivize b,projectivize c}`,
  REPEAT GEN_TAC THEN ONCE_REWRITE_TAC[COLLINEAR_3] THEN
  REWRITE_TAC[GSYM DOT_CAUCHY_SCHWARZ_EQUAL] THEN
  REWRITE_TAC[COLLINEAR_BRACKET; projectivize; o_THM; bracket] THEN
  MATCH_MP_TAC EQ_TRANS THEN
  EXISTS_TAC `det(vector[homogenize a; homogenize b; homogenize c]) = &0` THEN
  CONJ_TAC THENL
   [REWRITE_TAC[homogenize; DOT_2; VECTOR_SUB_COMPONENT; DET_3; VECTOR_3] THEN
    CONV_TAC "100/pascal.ml:REAL_RING" REAL_RING;
    MAP_EVERY (MP_TAC o C SPEC PARALLEL_PROJP_HOMOP)
     [`homogenize a`; `homogenize b`; `homogenize c`] THEN
    MAP_EVERY (MP_TAC o C SPEC HOMOGENIZE_NONZERO)
     [`a:real^2`; `b:real^2`; `c:real^2`] THEN
    MAP_EVERY (MP_TAC o CONJUNCT1 o C SPEC homop)
     [`projp(homogenize a)`; `projp(homogenize b)`; `projp(homogenize c)`] THEN
    REWRITE_TAC[desargues_parallel; cross; CART_EQ; DIMINDEX_3; FORALL_3; VECTOR_3;
                DET_3; VEC_COMPONENT] THEN
    CONV_TAC "100/pascal.ml:REAL_RING" REAL_RING]);;

let AFFINE_PROJECTIVE_CONIC = prove
 (`!con. affine_conic con <=> ?con'. projective_conic con' /\
                                     con = {x | projectivize x IN con'}`,
  REWRITE_TAC[affine_conic; projective_conic; homogeneous_conic] THEN
  GEN_TAC THEN REWRITE_TAC[LEFT_AND_EXISTS_THM] THEN
  ONCE_REWRITE_TAC[MESON[]
   `(?con' con a b c d e f. P con' con a b c d e f) <=>
    (?a b d e f c con' con. P con' con a b c d e f)`] THEN
  MAP_EVERY (fun s ->
   AP_TERM_TAC THEN GEN_REWRITE_TAC "100/pascal.ml:I" I [FUN_EQ_THM] THEN
   X_GEN_TAC(mk_var(s,`:real`)) THEN REWRITE_TAC[])
   ["a"; "b"; "c"; "d"; "e"; "f"] THEN
  REWRITE_TAC[RIGHT_EXISTS_AND_THM; UNWIND_THM2; GSYM CONJ_ASSOC] THEN
  REWRITE_TAC[IN_ELIM_THM; projectivize; o_THM] THEN
  BINOP_TAC THENL [CONV_TAC "100/pascal.ml:TAUT" TAUT; AP_TERM_TAC] THEN
  REWRITE_TAC[EXTENSION] THEN X_GEN_TAC `x:real^2` THEN
  MP_TAC(SPEC `x:real^2` HOMOGENIZE_NONZERO) THEN
  DISCH_THEN(MP_TAC o MATCH_MP PARALLEL_PROJP_HOMOP_EXPLICIT) THEN
  DISCH_THEN(X_CHOOSE_THEN `k:real` STRIP_ASSUME_TAC) THEN
  ASM_REWRITE_TAC[IN_ELIM_THM; VECTOR_MUL_COMPONENT] THEN
  REWRITE_TAC[homogenize; VECTOR_3] THEN
  UNDISCH_TAC `~(k = &0)` THEN CONV_TAC "100/pascal.ml:REAL_RING" REAL_RING);;

(* ------------------------------------------------------------------------- *)
(* Hence Pascal's theorem for the affine plane.                              *)
(* ------------------------------------------------------------------------- *)

let PASCAL_AFFINE = prove
 (`!con x1 x2 x3 x4 x5 x6 x7 x8 x9:real^2.
        ~collinear {x1,x2,x4} /\
        ~collinear {x1,x2,x5} /\
        ~collinear {x1,x2,x6} /\
        ~collinear {x1,x3,x4} /\
        ~collinear {x1,x3,x5} /\
        ~collinear {x1,x3,x6} /\
        ~collinear {x2,x3,x4} /\
        ~collinear {x2,x3,x5} /\
        ~collinear {x2,x3,x6} /\
        ~collinear {x4,x5,x1} /\
        ~collinear {x4,x5,x2} /\
        ~collinear {x4,x5,x3} /\
        ~collinear {x4,x6,x1} /\
        ~collinear {x4,x6,x2} /\
        ~collinear {x4,x6,x3} /\
        ~collinear {x5,x6,x1} /\
        ~collinear {x5,x6,x2} /\
        ~collinear {x5,x6,x3}
        ==> affine_conic con /\
            x1 IN con /\ x2 IN con /\ x3 IN con /\
            x4 IN con /\ x5 IN con /\ x6 IN con /\
            collinear {x1,x9,x5} /\
            collinear {x1,x8,x6} /\
            collinear {x2,x9,x4} /\
            collinear {x2,x7,x6} /\
            collinear {x3,x8,x4} /\
            collinear {x3,x7,x5}
            ==> collinear {x7,x8,x9}`,
  REWRITE_TAC[COLLINEAR_PROJECTIVIZE; AFFINE_PROJECTIVE_CONIC] THEN
  REPEAT(GEN_TAC ORELSE DISCH_TAC) THEN
  FIRST_X_ASSUM(MATCH_MP_TAC o MATCH_MP PASCAL) THEN
  ASM_REWRITE_TAC[] THEN
  FIRST_X_ASSUM(CONJUNCTS_THEN2 MP_TAC ASSUME_TAC) THEN
  MATCH_MP_TAC MONO_EXISTS THEN ASM SET_TAC[]);;

(* ------------------------------------------------------------------------- *)
(* Special case of a circle where nondegeneracy is simpler.                  *)
(* ------------------------------------------------------------------------- *)

let COLLINEAR_NOT_COCIRCULAR = prove
 (`!r c x y z:real^2.
        dist(c,x) = r /\ dist(c,y) = r /\ dist(c,z) = r /\
        ~(x = y) /\ ~(x = z) /\ ~(y = z)
        ==> ~collinear {x,y,z}`,
  ONCE_REWRITE_TAC[GSYM VECTOR_SUB_EQ] THEN
  REWRITE_TAC[GSYM DOT_EQ_0] THEN
  ONCE_REWRITE_TAC[COLLINEAR_3] THEN
  REWRITE_TAC[GSYM DOT_CAUCHY_SCHWARZ_EQUAL; DOT_2] THEN
  REWRITE_TAC[dist; NORM_EQ_SQUARE; CART_EQ; DIMINDEX_2; FORALL_2;
              DOT_2; VECTOR_SUB_COMPONENT] THEN
  CONV_TAC "100/pascal.ml:REAL_RING" REAL_RING);;

let PASCAL_AFFINE_CIRCLE = prove
 (`!c r x1 x2 x3 x4 x5 x6 x7 x8 x9:real^2.
        PAIRWISE (\x y. ~(x = y)) [x1;x2;x3;x4;x5;x6] /\
        dist(c,x1) = r /\ dist(c,x2) = r /\ dist(c,x3) = r /\
        dist(c,x4) = r /\ dist(c,x5) = r /\ dist(c,x6) = r /\
        collinear {x1,x9,x5} /\
        collinear {x1,x8,x6} /\
        collinear {x2,x9,x4} /\
        collinear {x2,x7,x6} /\
        collinear {x3,x8,x4} /\
        collinear {x3,x7,x5}
        ==> collinear {x7,x8,x9}`,
  GEN_TAC THEN GEN_TAC THEN
  MP_TAC(SPEC `{x:real^2 | dist(c,x) = r}` PASCAL_AFFINE) THEN
  REPEAT(MATCH_MP_TAC MONO_FORALL THEN GEN_TAC) THEN
  REWRITE_TAC[PAIRWISE; ALL; IN_ELIM_THM] THEN
  GEN_REWRITE_TAC "100/pascal.ml:LAND_CONV" LAND_CONV [IMP_IMP] THEN
  DISCH_TAC THEN STRIP_TAC THEN FIRST_X_ASSUM MATCH_MP_TAC THEN
  ASM_REWRITE_TAC[] THEN CONJ_TAC THENL
   [REPEAT CONJ_TAC THEN MATCH_MP_TAC COLLINEAR_NOT_COCIRCULAR THEN
    MAP_EVERY EXISTS_TAC [`r:real`; `c:real^2`] THEN ASM_REWRITE_TAC[];
    REWRITE_TAC[affine_conic; dist; NORM_EQ_SQUARE] THEN
    ASM_CASES_TAC `&0 <= r` THEN ASM_REWRITE_TAC[] THENL
     [MAP_EVERY EXISTS_TAC
       [`&1`; `&1`; `&0`; `-- &2 * (c:real^2)$1`; `-- &2 * (c:real^2)$2`;
        `(c:real^2)$1 pow 2 + (c:real^2)$2 pow 2 - r pow 2`] THEN
      REWRITE_TAC[EXTENSION; IN_ELIM_THM] THEN
      REWRITE_TAC[DOT_2; VECTOR_SUB_COMPONENT] THEN REAL_ARITH_TAC;
      REPLICATE_TAC 5 (EXISTS_TAC `&0`) THEN EXISTS_TAC `&1` THEN
      REWRITE_TAC[EXTENSION; IN_ELIM_THM] THEN REAL_ARITH_TAC]]);;
Pb_printer.clear_file_tags();;
