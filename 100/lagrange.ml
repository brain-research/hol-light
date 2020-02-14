(* ========================================================================= *)
(* Very trivial group theory, just to reach Lagrange theorem.                *)
(* ========================================================================= *)

set_jrh_lexer;;
open Lib;;
open Printer;;
open Parser;;
open Equal;;
open Bool;;
open Drule;;
open Tactics;;
open Simp;;
open Theorems;;
open Class;;
open Meson;;
open Pair;;
open Sets;;

open Prime;;
(* ------------------------------------------------------------------------- *)
(* Definition of what a group is.                                            *)
(* ------------------------------------------------------------------------- *)

(* Copy the same precedence and associativity as normal ** operator. *)
parse_as_infix("lagrange_group", (20, "right"));;

let group = new_definition
  `group(g,( lagrange_group ),i,(e:A)) <=>
    (e IN g) /\ (!x. x IN g ==> i(x) IN g) /\
    (!x y. x IN g /\ y IN g ==> (x lagrange_group y) IN g) /\
    (!x y z. x IN g /\ y IN g /\ z IN g ==> (x lagrange_group (y lagrange_group z) = (x lagrange_group y) lagrange_group z)) /\
    (!x. x IN g ==> (x lagrange_group e = x) /\ (e lagrange_group x = x)) /\
    (!x. x IN g ==> (x lagrange_group i(x) = e) /\ (i(x) lagrange_group x = e))`;;

(* ------------------------------------------------------------------------- *)
(* Notion of a subgroup.                                                     *)
(* ------------------------------------------------------------------------- *)

let subgroup = new_definition
  `subgroup h (g,( lagrange_group ),i,(e:A)) <=> h SUBSET g /\ group(h,( lagrange_group ),i,e)`;;

(* ------------------------------------------------------------------------- *)
(* Lagrange theorem, introducing the coset representatives.                  *)
(* ------------------------------------------------------------------------- *)

let GROUP_LAGRANGE_COSETS = prove
 (`!g h ( lagrange_group ) i e.
        group (g,( lagrange_group ),i,e:A) /\ subgroup h (g,( lagrange_group ),i,e) /\ FINITE g
        ==> ?q. (CARD(g) = CARD(q) * CARD(h)) /\
                (!b. b IN g ==> ?a x. a IN q /\ x IN h /\ (b = a lagrange_group x))`,
  REPEAT GEN_TAC THEN REWRITE_TAC[group; subgroup; SUBSET] THEN STRIP_TAC THEN
  ABBREV_TAC
   `coset = \a:A. {b:A | b IN g /\ (?x:A. x IN h /\ (b = a lagrange_group x))}` THEN
  SUBGOAL_THEN `!a:A. a IN g ==> a IN (coset a)` ASSUME_TAC THENL
   [GEN_TAC THEN DISCH_TAC THEN EXPAND_TAC "coset" THEN
    ASM_SIMP_TAC[IN_ELIM_THM] THEN ASM_MESON_TAC[];
    ALL_TAC] THEN
  SUBGOAL_THEN `FINITE(h:A->bool)` ASSUME_TAC THENL
   [ASM_MESON_TAC[FINITE_SUBSET; SUBSET]; ALL_TAC] THEN
  SUBGOAL_THEN `!a. FINITE((coset:A->A->bool) a)` ASSUME_TAC THENL
   [GEN_TAC THEN EXPAND_TAC "coset" THEN
    MATCH_MP_TAC FINITE_SUBSET THEN EXISTS_TAC `g:A->bool` THEN
    ASM_SIMP_TAC[IN_ELIM_THM; SUBSET];
    ALL_TAC] THEN
  SUBGOAL_THEN
   `!a:A x:A y. a IN g /\ x IN g /\ y IN g /\ ((a lagrange_group x) :A = a lagrange_group y)
                ==> (x = y)`
  ASSUME_TAC THENL
   [REPEAT STRIP_TAC THEN
    SUBGOAL_THEN `(e:A lagrange_group x:A):A = e lagrange_group y` (fun th -> ASM_MESON_TAC[th]) THEN
    SUBGOAL_THEN
     `((i(a):A lagrange_group a:A) lagrange_group x) = (i(a) lagrange_group a) lagrange_group y`
     (fun th -> ASM_MESON_TAC[th]) THEN
    ASM_MESON_TAC[];
    ALL_TAC] THEN
  SUBGOAL_THEN `!a:A. a IN g ==> (CARD(coset a :A->bool) = CARD(h:A->bool))`
  ASSUME_TAC THENL
   [REPEAT STRIP_TAC THEN
    SUBGOAL_THEN
     `(coset:A->A->bool) (a:A) = IMAGE (\x. a lagrange_group x) (h:A->bool)`
    SUBST1_TAC THENL
     [EXPAND_TAC "coset" THEN
      REWRITE_TAC[EXTENSION; IN_ELIM_THM; IN_IMAGE; IN_ELIM_THM] THEN
      ASM_MESON_TAC[];
      ALL_TAC] THEN
    MATCH_MP_TAC CARD_IMAGE_INJ THEN ASM_REWRITE_TAC[] THEN
    ASM_MESON_TAC[];
    ALL_TAC] THEN
  SUBGOAL_THEN `!x:A y. x IN g /\ y IN g ==> (i(x lagrange_group y) = i(y) lagrange_group i(x))`
  ASSUME_TAC THENL
   [REPEAT STRIP_TAC THEN
    FIRST_ASSUM MATCH_MP_TAC THEN
    EXISTS_TAC `(x:A lagrange_group y:A) :A` THEN ASM_SIMP_TAC[] THEN
    MATCH_MP_TAC EQ_TRANS THEN
    EXISTS_TAC `(x:A lagrange_group (y lagrange_group i(y))) lagrange_group i(x)` THEN
    ASM_MESON_TAC[];
    ALL_TAC] THEN
  SUBGOAL_THEN `!x:A. x IN g ==> (i(i(x)) = x)` ASSUME_TAC THENL
   [REPEAT STRIP_TAC THEN FIRST_ASSUM MATCH_MP_TAC THEN
    EXISTS_TAC `(i:A->A)(x)` THEN ASM_MESON_TAC[];
    ALL_TAC] THEN
  SUBGOAL_THEN
   `!a b. a IN g /\ b IN g
          ==> ((coset:A->A->bool) a = coset b) \/
              ((coset a) INTER (coset b) = {})`
  ASSUME_TAC THENL
   [REPEAT STRIP_TAC THEN
    ASM_CASES_TAC `((i:A->A)(b) lagrange_group a:A) IN (h:A->bool)` THENL
     [DISJ1_TAC THEN EXPAND_TAC "coset" THEN
      REWRITE_TAC[EXTENSION; IN_ELIM_THM] THEN
      GEN_TAC THEN AP_TERM_TAC THEN
      SUBGOAL_THEN
       `!x:A. x IN h ==> (b lagrange_group (i(b) lagrange_group a:A) lagrange_group x = a lagrange_group x) /\
                         (a lagrange_group i(i(b) lagrange_group a) lagrange_group x = b lagrange_group x)`
       (fun th -> EQ_TAC THEN REPEAT STRIP_TAC THEN
          ASM_REWRITE_TAC[] THEN ASM_MESON_TAC[th]) THEN
      ASM_SIMP_TAC[];
      ALL_TAC] THEN
    DISJ2_TAC THEN REWRITE_TAC[EXTENSION; NOT_IN_EMPTY; IN_INTER] THEN
    X_GEN_TAC `x:A` THEN EXPAND_TAC "coset" THEN REWRITE_TAC[IN_ELIM_THM] THEN
    REWRITE_TAC[TAUT `(a /\ b) /\ (a /\ c) <=> a /\ b /\ c`] THEN
    DISCH_THEN(CONJUNCTS_THEN2 ASSUME_TAC MP_TAC) THEN
    DISCH_THEN(CONJUNCTS_THEN2 (X_CHOOSE_THEN `y:A` STRIP_ASSUME_TAC)
                               (X_CHOOSE_THEN `z:A` STRIP_ASSUME_TAC)) THEN
    SUBGOAL_THEN `(i(b:A) lagrange_group a lagrange_group y):A = i(b) lagrange_group b lagrange_group z` ASSUME_TAC THENL
     [ASM_MESON_TAC[]; ALL_TAC] THEN
    SUBGOAL_THEN `(i(b:A) lagrange_group a:A lagrange_group y):A = e lagrange_group z` ASSUME_TAC THENL
     [ASM_MESON_TAC[]; ALL_TAC] THEN
    SUBGOAL_THEN `(i(b:A) lagrange_group a:A lagrange_group y):A = z` ASSUME_TAC THENL
     [ASM_MESON_TAC[]; ALL_TAC] THEN
    SUBGOAL_THEN `((i(b:A) lagrange_group a:A) lagrange_group y):A = z` ASSUME_TAC THENL
     [ASM_MESON_TAC[]; ALL_TAC] THEN
    SUBGOAL_THEN `((i(b:A) lagrange_group a:A) lagrange_group y) lagrange_group i(y) = z lagrange_group i(y)` ASSUME_TAC THENL
     [ASM_MESON_TAC[]; ALL_TAC] THEN
    SUBGOAL_THEN `(i(b:A) lagrange_group a:A) lagrange_group (y lagrange_group i(y)) = z lagrange_group i(y)` ASSUME_TAC THENL
     [ASM_MESON_TAC[]; ALL_TAC] THEN
    SUBGOAL_THEN `(i(b:A) lagrange_group a:A) lagrange_group e = z lagrange_group i(y)` ASSUME_TAC THENL
     [ASM_MESON_TAC[]; ALL_TAC] THEN
    SUBGOAL_THEN `(i(b:A) lagrange_group a:A):A = z lagrange_group i(y)` ASSUME_TAC THENL
     [ASM_MESON_TAC[]; ALL_TAC] THEN
    ASM_MESON_TAC[];
    ALL_TAC] THEN
  EXISTS_TAC `{c:A | ?a:A. a IN g /\ (c = (@)(coset a))}` THEN
  MATCH_MP_TAC(TAUT `a /\ (a ==> b) ==> b /\ a`) THEN CONJ_TAC THENL
   [X_GEN_TAC `b:A` THEN DISCH_TAC THEN
    EXISTS_TAC `(@)((coset:A->A->bool) b)` THEN
    REWRITE_TAC[RIGHT_EXISTS_AND_THM] THEN CONJ_TAC THENL
     [REWRITE_TAC[IN_ELIM_THM] THEN EXISTS_TAC `b:A` THEN
      ASM_REWRITE_TAC[];
      ALL_TAC] THEN
    SUBGOAL_THEN `(@)((coset:A->A->bool) b) IN (coset b)` MP_TAC THENL
     [REWRITE_TAC[IN] THEN MATCH_MP_TAC SELECT_AX THEN
      ASM_MESON_TAC[IN];
      ALL_TAC] THEN
    FIRST_ASSUM(fun th -> GEN_REWRITE_TAC "100/lagrange.ml:(LAND_CONV o RAND_CONV o RATOR_CONV)"  (LAND_CONV o RAND_CONV o RATOR_CONV)
                         [SYM th]) THEN
    REWRITE_TAC[] THEN
    ABBREV_TAC `C = (@)((coset:A->A->bool) b)` THEN
    REWRITE_TAC[IN_ELIM_THM] THEN
    DISCH_THEN(CONJUNCTS_THEN2 ASSUME_TAC
     (X_CHOOSE_THEN `c:A` STRIP_ASSUME_TAC)) THEN
    EXISTS_TAC `(i:A->A)(c)` THEN ASM_SIMP_TAC[] THEN
    ASM_MESON_TAC[];
    ALL_TAC] THEN
  ABBREV_TAC `q = {c:A | ?a:A. a IN g /\ (c = (@)(coset a))}` THEN
  DISCH_TAC THEN
  SUBGOAL_THEN
   `!a:A b. a IN g /\ b IN g /\ a IN coset(b) ==> b IN coset(a)`
  ASSUME_TAC THENL
   [REPEAT GEN_TAC THEN EXPAND_TAC "coset" THEN
    REWRITE_TAC[IN_ELIM_THM] THEN REWRITE_TAC[GSYM CONJ_ASSOC] THEN
    REPEAT(DISCH_THEN(CONJUNCTS_THEN2 ASSUME_TAC MP_TAC)) THEN
    DISCH_THEN(X_CHOOSE_THEN `c:A` STRIP_ASSUME_TAC) THEN
    ASM_REWRITE_TAC[] THEN EXISTS_TAC `(i:A->A) c` THEN
    ASM_REWRITE_TAC[] THEN ASM_MESON_TAC[];
    ALL_TAC] THEN
  SUBGOAL_THEN
   `!a:A b c. a IN coset(b) /\ b IN coset(c) /\ c IN g ==> a IN coset(c)`
  ASSUME_TAC THENL
   [REPEAT GEN_TAC THEN EXPAND_TAC "coset" THEN
    REWRITE_TAC[IN_ELIM_THM] THEN
    STRIP_TAC THEN ASM_REWRITE_TAC[] THEN ASM_MESON_TAC[];
    ALL_TAC] THEN
  SUBGOAL_THEN `!a:A b:A. a IN coset(b) ==> a IN g` ASSUME_TAC THENL
   [EXPAND_TAC "coset" THEN REWRITE_TAC[IN_ELIM_THM] THEN MESON_TAC[];
    ALL_TAC] THEN
  SUBGOAL_THEN
   `!a:A b. a IN coset(b) /\ b IN g ==> (coset a = coset b)`
  ASSUME_TAC THENL
   [REWRITE_TAC[EXTENSION] THEN
    MAP_EVERY UNDISCH_TAC
     [`!a:A b:A. a IN coset(b) ==> a IN g`;
      `!a:A b c. a IN coset(b) /\ b IN coset(c) /\ c IN g ==> a IN coset(c)`;
      `!a:A b. a IN g /\ b IN g /\ a IN coset(b) ==> b IN coset(a)`] THEN
    MESON_TAC[];
    ALL_TAC] THEN
  SUBGOAL_THEN `!a:A. a IN g ==> (@)(coset a):A IN (coset a)` ASSUME_TAC THENL
   [REPEAT STRIP_TAC THEN UNDISCH_TAC `!a:A. a IN g ==> a IN coset a` THEN
    DISCH_THEN(MP_TAC o SPEC `a:A`) THEN
    ASM_REWRITE_TAC[] THEN REWRITE_TAC[IN; SELECT_AX];
    ALL_TAC] THEN
  SUBGOAL_THEN `!a:A. a IN q ==> a IN g` ASSUME_TAC THENL
   [GEN_TAC THEN EXPAND_TAC "q" THEN REWRITE_TAC[IN_ELIM_THM] THEN
    STRIP_TAC THEN ASM_REWRITE_TAC[] THEN ASM_MESON_TAC[];
    ALL_TAC] THEN
  SUBGOAL_THEN
   `!a:A x:A a' x'. a IN q /\ a' IN q /\ x IN h /\ x' IN h /\
                    ((a' lagrange_group x') :A = a lagrange_group x) ==> (a' = a) /\ (x' = x)`
  ASSUME_TAC THENL
   [REPEAT GEN_TAC THEN EXPAND_TAC "q" THEN REWRITE_TAC[IN_ELIM_THM] THEN
    MATCH_MP_TAC(TAUT `(c ==> a /\ b ==> d) ==> a /\ b /\ c ==> d`) THEN
    STRIP_TAC THEN
    DISCH_THEN(CONJUNCTS_THEN2
     (X_CHOOSE_THEN `a1:A` STRIP_ASSUME_TAC)
     (X_CHOOSE_THEN `a2:A` STRIP_ASSUME_TAC)) THEN
    SUBGOAL_THEN `a:A IN g /\ a' IN g` STRIP_ASSUME_TAC THENL
     [ASM_REWRITE_TAC[] THEN ASM_MESON_TAC[]; ALL_TAC] THEN
    MATCH_MP_TAC(TAUT `(a ==> b) /\ a ==> a /\ b`) THEN
    CONJ_TAC THENL [ASM_MESON_TAC[]; ALL_TAC] THEN
    SUBGOAL_THEN
     `((coset:A->A->bool) a1 = coset a) /\ (coset a2 = coset a')`
    MP_TAC THENL
     [CONJ_TAC THEN CONV_TAC "100/lagrange.ml:SYM_CONV" SYM_CONV THEN FIRST_ASSUM MATCH_MP_TAC THEN
      ASM_SIMP_TAC[];
      ALL_TAC] THEN
    DISCH_THEN(CONJUNCTS_THEN SUBST_ALL_TAC) THEN
    ONCE_ASM_REWRITE_TAC[] THEN AP_TERM_TAC THEN
    FIRST_ASSUM MATCH_MP_TAC THEN ASM_REWRITE_TAC[] THEN
    EXPAND_TAC "coset" THEN REWRITE_TAC[IN_ELIM_THM] THEN
    ASM_REWRITE_TAC[] THEN EXISTS_TAC `(x:A lagrange_group (i:A->A)(x')):A` THEN
    ASM_SIMP_TAC[] THEN UNDISCH_TAC `(a':A lagrange_group x':A):A = a lagrange_group x` THEN
    DISCH_THEN(MP_TAC o C AP_THM `(i:A->A) x'` o AP_TERM `(lagrange_group):A->A->A`) THEN
    DISCH_THEN(SUBST1_TAC o SYM) THEN ASM_MESON_TAC[];
    ALL_TAC] THEN
  SUBGOAL_THEN `g = IMAGE (\(a:A,x:A). (a lagrange_group x):A) {(a,x) | a IN q /\ x IN h}`
  SUBST1_TAC THENL
   [REWRITE_TAC[EXTENSION; IN_IMAGE; IN_ELIM_THM] THEN
    REWRITE_TAC[EXISTS_PAIR_THM] THEN
    CONV_TAC "100/lagrange.ml:(ONCE_DEPTH_CONV GEN_BETA_CONV)" (ONCE_DEPTH_CONV GEN_BETA_CONV) THEN
    REWRITE_TAC[PAIR_EQ] THEN
    REWRITE_TAC[CONJ_ASSOC; ONCE_REWRITE_RULE[CONJ_SYM] UNWIND_THM1] THEN
    ASM_MESON_TAC[];
    ALL_TAC] THEN
  MATCH_MP_TAC EQ_TRANS THEN
  EXISTS_TAC `CARD {(a:A,x:A) | a IN q /\ x IN h}` THEN CONJ_TAC THENL
   [MATCH_MP_TAC CARD_IMAGE_INJ THEN CONJ_TAC THENL
     [REWRITE_TAC[FORALL_PAIR_THM; IN_ELIM_THM] THEN
      CONV_TAC "100/lagrange.ml:(ONCE_DEPTH_CONV GEN_BETA_CONV)" (ONCE_DEPTH_CONV GEN_BETA_CONV) THEN
      REWRITE_TAC[PAIR_EQ] THEN REPEAT GEN_TAC THEN
      REPEAT(DISCH_THEN(CONJUNCTS_THEN2 STRIP_ASSUME_TAC MP_TAC)) THEN
      ASM_REWRITE_TAC[] THEN ASM_MESON_TAC[];
      ALL_TAC] THEN
    MATCH_MP_TAC FINITE_PRODUCT THEN CONJ_TAC THEN
    MATCH_MP_TAC FINITE_SUBSET THEN EXISTS_TAC `g:A->bool` THEN
    ASM_REWRITE_TAC[SUBSET];
    ALL_TAC] THEN
  MATCH_MP_TAC CARD_PRODUCT THEN CONJ_TAC THEN
  MATCH_MP_TAC FINITE_SUBSET THEN EXISTS_TAC `g:A->bool` THEN
  ASM_REWRITE_TAC[SUBSET]);;

(* ------------------------------------------------------------------------- *)
(* Traditional statement is only part of this.                               *)
(* ------------------------------------------------------------------------- *)

let GROUP_LAGRANGE = prove
 (`!g h ( lagrange_group ) i e.
        group (g,( lagrange_group ),i,e:A) /\ subgroup h (g,( lagrange_group ),i,e) /\ FINITE g
        ==> (CARD h) divides (CARD g)`,
  REPEAT GEN_TAC THEN DISCH_THEN(MP_TAC o MATCH_MP GROUP_LAGRANGE_COSETS) THEN
  MESON_TAC[DIVIDES_LMUL; DIVIDES_REFL]);;
