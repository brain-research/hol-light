set_jrh_lexer;;
open Fusion;;
open Printer;;
open Tactics;;
open Theorem_fingerprint;;
open List;;

let MAX_TERM_LENGTH = 10000;;  (* 10k characters*)

type response_atom = String of string | Int of int

let apply_tactic tc = let _ = e (Parse_tactic.parse tc) in []
let rotate n = let _ = r n in []
let undo() = let _ = b() in []
let register_last_thm() =
  let thm = fst (top_thm()) in
  register_thm thm;;

(* -------------------------------------------------------------------------- *
 * This function is hacky. We need to act differently depending on whether    *
 * the definition exists already. thm_tm and def_tm are the same, just that   *
 * thm_tm marks the defined symbol as a constant.                             *
 * -------------------------------------------------------------------------- *)

let define_inductive (tm: term) : unit =
  let th1, th2, th3 = Ind_defs.new_inductive_definition tm in
  List.map register_thm [th1; th2; th3]; ();;

let specification (constants: string list) (spec_thm_fingerprint: int) : int =
  let spec_thm : thm = thm_of_index spec_thm_fingerprint in
  register_thm (Nums.new_specification constants spec_thm);;

let current_goals() = try
    let (_,gs,_)::_ = !current_goalstack in gs
  with Match_failure _ -> []
let send_goals gs =
  let str_of_tm t =
    let raw_string = Printer.encode_term t in
    (if String.length raw_string > MAX_TERM_LENGTH then
      raise (Failure "HOL Light term in resulting goal exceeds MAX_TERM_LENGTH."));
    String raw_string in
  let serialize (asl,w) = (* Format: [pretty_printed, conclusion, assumptions]*)
    let terms = w::(List.map (fun (_,t) -> Fusion.concl t) asl) in
    let strings = String (string_of_goal (asl, w))
                  :: List.map str_of_tm terms in
    (Int (List.length strings)) :: strings in
  (Int (List.length gs))::(List.flatten (List.map serialize gs))
let send_thm th = (* Format: [pretty_printed, conclusion, hypotheses] *)
  let str_of_tm t =
    let raw_string = Printer.encode_term t in
    (if String.length raw_string > MAX_TERM_LENGTH then
      raise (Failure "HOL Light term in resulting goal exceeds MAX_TERM_LENGTH."));
    String raw_string in
  let hyps, w = dest_thm(th) in
  let terms = w::hyps in
  let strings = String (string_of_thm th)
                  :: List.map str_of_tm terms in
  (Int (List.length strings)) :: strings
let repeat n f =
  let rec repeat_tr n l = if n > 0 then repeat_tr (n-1) (f()::l) else l in
  List.rev (repeat_tr n [])
let receive_string_list() = repeat (Comms.receive_int()) Comms.receive_string
let to_term_list string_list =
  (* This produces a (term list * term) which can be passed to set_goal *)
  let c::asl = List.map Parser.decode_term string_list in (asl, c)
let to_goal string_list =
  (* This produces a goal = (string * thm) list * term. Internally HOL
   * represents the hypotheses of a goal as theorems of the form t |- t, which
   * we generate using ASSUME as in HOL's set_goal function. *)
  let (asl, w) = to_term_list string_list in
  (List.map (fun t -> ("", Fusion.ASSUME t)) asl, w)

let exit_on_timeout () =
  Sys.signal Sys.sigint (Sys.Signal_handle (fun _ -> raise Sys.Break));;

let ignore_timeout () =
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> ()));;

let with_timeout f x =
  exit_on_timeout ();
  let res = try f x
    with e ->
      ignore_timeout ();
      raise e in
  ignore_timeout ();
  res;;

(******************* Support negating a goal. *******************)

(* Tactic to bind a free variable in a goal. *)
let SPEC_OUTERMOST_TAC g = SPEC_TAC (hd (frees (snd g)), hd (frees (snd g))) g;;

(* Tactic to move to conclusion first assumption is the assumption list.*)
let UNDISCH_HD_TAC (asm, c) = UNDISCH_TAC (concl (snd (hd asm))) (asm, c);;

(* Takes as input a goal, and returns a term which is supposed to represent
 * the negation of the goal *)
let negate_goal input_goal =
  let _, no_asm_subgoals, _ = REPEAT UNDISCH_HD_TAC input_goal in
  let no_asm_goal = (match no_asm_subgoals with
    | g::[] -> g
    | _ -> failwith "negate_goal: expected 1 subgoal pulling assumptions."
  ) in
  let _, no_frees_subgoals, _ = REPEAT SPEC_OUTERMOST_TAC no_asm_goal in
  let input_goal_as_term = (match no_frees_subgoals with
    | (_, t)::[] -> t
    | _ -> failwith "negate_goal: expected 1 subgoal with no assumptions here"
  ) in
  Bool.mk_neg input_goal_as_term;;

(******************* handle_request *******************)

let kOk = Int 0
let kError = Int 1
let handle_request() =
  try
    let result = kOk::(
    match Comms.receive_int() with
    | 0 (* = kSetGoal *) ->
        let gs = receive_string_list() in
        let _ = set_goal (to_term_list gs) in []
    | 1 (* = kGetGoals *) -> send_goals (current_goals())
    | 2 (* = kRotate *) -> let n = Comms.receive_int() in rotate n
    | 3 (* = kApplyTactic *) ->
        let tc = Comms.receive_string() in with_timeout apply_tactic tc
    | 4 (* = kUndo *) -> undo()
    | 5 (* = kRegisterLastTheorem *) -> register_last_thm (); []
    | 6 (* = kDefine *) ->
        let definition_type = Comms.receive_string() in
        let def_term = Comms.receive_string() in
        (match definition_type with
          "BASIC" ->
          let tm : term = Parser.decode_term def_term in
          register_thm (Bool.log_new_basic_definition tm);
          ()
        | "DRULE" ->
          let tm : term = Parser.decode_term def_term in
          register_thm (Drule.new_definition tm);
          ()
        | "PAIR" ->
          let tm : term = Parser.decode_term def_term in
          register_thm (Pair.new_definition tm);
          ()
        | "SPEC" ->
            let spec_thm: int = Comms.receive_int() in
            let num_constants: int = Comms.receive_int() in
            let rec read_constants n =
              if n<=0 then []
              else (let c = Comms.receive_string() in
                    c::read_constants (n-1)) in
            let constants : string list = read_constants num_constants in
            specification constants spec_thm;
            ()
        | "RECURSIVE" ->
            let tm : term = Parser.decode_term def_term in
            (*Printf.eprintf "Decoded term: %s\n%!" (str_of_sexp (sexp_term tm));*)
            let rec_thm_fp: int = Comms.receive_int() in
            let rec_thm: thm = thm_of_index rec_thm_fp in
            let ret_thm: thm = Recursion.new_recursive_definition rec_thm tm in
            register_thm ret_thm;
            ()
        | "INDUCTIVE" ->
          let tm : term = Parser.decode_term def_term in
          define_inductive tm
        | "DEFINE" ->
          let tm : term = Parser.decode_term def_term in
          let ret_thm = Define.define tm in
          register_thm ret_thm;
          ()
        ); []  (* end of match definition_type *)
    | 7 (* = kSetEncoding *) ->
        let _ = Printer.current_encoding := (match Comms.receive_int() with
        | 1 (* = TE_PRETTY *) -> Printer.Pretty
        | 2 (* = TE_SEXP *) -> Printer.Sexp) in []
    | 8 (* = kApplyTacticToGoal *) ->
        let gs = receive_string_list() in
        let ts = Comms.receive_string() in
        (* We defer parsing the goals so that a parse failure doesn't cause
         * the communication to get out of sync. *)
        (try
          with_timeout (fun (gs, ts) ->
              (* Wrapping not only the tactic application, but also parsing of
                 goals and tactics into the timeout-sensitive part, as we
                 observed parsing to take longer than the timeout in extreme
                 cases. This led to unintentionally ignoring the timeout.     *)
              let (g, t) = (to_goal gs, Parse_tactic.parse ts) in
              let (_, gl, _) = t g in
              kOk::(send_goals gl)
            ) (gs, ts)
        with e -> [kError;String (Printexc.to_string e)])
    | 9 (* = kRegisterTheorem *) ->
        let gs = receive_string_list() in
        let thm_to_register = Drule.mk_thm (to_term_list gs) in
(*         let fingerprint = Theorem_fingerprint.fingerprint(thm_to_register) in *)
        let fingerprint = register_thm thm_to_register in
(*         Theorem_fingerprint.index_thm fingerprint (thm_to_register); *)
        [Int fingerprint]
    | 10 (* = kCompareLastTheorem *) ->
        let gs = receive_string_list() in
        let thm_to_register = Drule.mk_thm (to_term_list gs) in
        let expected = Theorem_fingerprint.fingerprint(thm_to_register) in
        let fingerprint = Theorem_fingerprint.fingerprint(fst (top_thm())) in
        (if fingerprint != expected then
          failwith ("Last theorem is not THM " ^ string_of_int expected ^
              " but " ^ string_of_int fingerprint ^
              "; Theorem: " ^ str_of_sexp (sexp_thm (fst (top_thm())))));
        []
    | 11 (* = kDefineType *) ->
        let tyname = Comms.receive_string() in
        let absname = Comms.receive_string() in
        let repname = Comms.receive_string() in
        let thm_arg_fp : int = Comms.receive_int() in
        let thm_arg : thm = thm_of_index thm_arg_fp in
        let ret_thm = Class.new_type_definition tyname (absname, repname) thm_arg in
        let fingerprint = register_thm ret_thm in
        [Int fingerprint]
    | 12 (* = kNegateGoal *) ->
        let gs = receive_string_list() in
        let negated_goal_as_term = negate_goal (to_goal gs) in
        [String (Printer.encode_term negated_goal_as_term)]
    | 13 (* = kApplyRule *) ->
        let rule_str = Comms.receive_string() in
        (try
          with_timeout (fun rule_str ->
          (* Wrapping the rule application in the timeout-sensitive part *)
            let th = Parse_tactic.parse_rule rule_str in
              kOk::(send_thm th)
            ) rule_str
        with e -> [kError;String (Printexc.to_string e)])
    ) in
    result

  with e -> [kError;String (Printexc.to_string e)]

let () =
(* Indicate to parent process that initialization is complete. The value sent is
 * arbitrary and discarded by the parent. *)
Comms.send_int 0;
ignore_timeout (); (* Ignore SIGINT while no request is being handled. *)
while true do
  let response = handle_request() in
  List.iter (function
    | String s -> Comms.send_string s
    | Int n -> Comms.send_int n) response
done
