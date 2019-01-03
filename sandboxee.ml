set_jrh_lexer;;
open Fusion;;
open Printer;;
open Tactics;;
open Theorem_fingerprint;;

type response_atom = String of string | Int of int

let apply_tactic tc = let _ = e (Parse_tactic.parse tc) in []
let rotate n = let _ = r n in []
let undo() = let _ = b() in []
let register_last_thm() = let thm = fst (top_thm()) in
  register_thm thm

(* -------------------------------------------------------------------------- *
 * This function is hacky. We need to act differently depending on whether    *
 * the definition exists already. thm_tm and def_tm are the same, just that   *
 * thm_tm marks the defined symbol as a constant.                             *
 * -------------------------------------------------------------------------- *)

let define_inductive (tm: term) : unit =
  let th1, th2, th3 = Ind_defs.new_inductive_definition tm in
  List.map register_thm [th1; th2; th3]; ();;

let specification (constants: string list) (spec_thm_fingerprint: int) : unit =
  let spec_thm : thm = thm_of_index spec_thm_fingerprint in
  register_thm (Nums.new_specification constants spec_thm);;

let current_goals() = try
    let (_,gs,_)::_ = !current_goalstack in gs
  with Match_failure _ -> []
let send_goals gs =
  let str_of_tm t = String (Printer.encode_term t) in
  let serialize (asl,w) =
    let g = w::(List.map (fun (_,t) -> Fusion.concl t) asl) in
    (Int (List.length g))::(List.map str_of_tm g) in
  (Int (List.length gs))::(List.flatten (List.map serialize gs))
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

let kOk = Int 0
let kError = Int 1
let handle_request() =
  try
    let old_handler = Sys.signal Sys.sigint
      (Sys.Signal_handle (fun _ -> raise Sys.Break)) in
    let result = kOk::(
    match Comms.receive_int() with
    | 0 (* = kSetGoal *) ->
        let gs = receive_string_list() in
        let _ = set_goal (to_term_list gs) in []
    | 1 (* = kGetGoals *) -> send_goals (current_goals())
    | 2 (* = kRotate *) -> let n = Comms.receive_int() in rotate n
    | 3 (* = kApplyTactic *) ->
        let tc = Comms.receive_string() in apply_tactic tc
    | 4 (* = kUndo *) -> undo()
    | 5 (* = kRegisterLastTheorem *) -> register_last_thm (); []
    | 6 (* = kDefine *) ->
        let definition_type = Comms.receive_string() in
        let def_term = Comms.receive_string() in
        (match definition_type with
          "BASIC" ->
          let tm : term = Parser.decode_term def_term in
          register_thm (Bool.log_new_basic_definition tm)
        | "DRULE" ->
          let tm : term = Parser.decode_term def_term in
          register_thm (Drule.new_definition tm)
        | "PAIR" ->
          let tm : term = Parser.decode_term def_term in
          register_thm (Pair.new_definition tm)
        | "SPEC" ->
            let spec_thm: int = Comms.receive_int() in
            let num_constants: int = Comms.receive_int() in
            let rec read_constants n =
              if n<=0 then []
              else (let c = Comms.receive_string() in
                    c::read_constants (n-1)) in
            let constants : string list = read_constants num_constants in
            specification constants spec_thm
        | "RECURSIVE" ->
            let tm : term = Parser.decode_term def_term in
            (*Printf.eprintf "Decoded term: %s\n%!" (str_of_sexp (sexp_term tm));*)
            let rec_thm_fp: int = Comms.receive_int() in
            let rec_thm: thm = thm_of_index rec_thm_fp in
            let ret_thm: thm = Recursion.new_recursive_definition rec_thm tm in
            register_thm ret_thm
        | "INDUCTIVE" ->
          let tm : term = Parser.decode_term def_term in
          define_inductive tm
        | "DEFINE" ->
          let tm : term = Parser.decode_term def_term in
          let ret_thm = Define.define tm in
          register_thm ret_thm); []
    | 7 (* = kSetEncoding *) ->
        let _ = Printer.current_encoding := (match Comms.receive_int() with
        | 1 (* = TE_PRETTY *) -> Printer.Pretty
        | 2 (* = TE_SEXP *) -> Printer.Sexp) in []
    | 8 (* = kApplyTacticToGoal *) ->
        let gs = receive_string_list() in
        let ts = Comms.receive_string() in
        (try
          (* We defer parsing the goals so that a parse failure doesn't cause
           * the communication to get out of sync. *)
          let (g, t) = (to_goal gs, Parse_tactic.parse ts) in
          let (_, gl, _) = t g in kOk::(send_goals gl)
        with e -> [kError;String (Printexc.to_string e)])
    | 9 (* = kRegisterTheorem *) ->
        let gs = receive_string_list() in
        let index = Comms.receive_int() in
        Theorem_fingerprint.index_thm index (Drule.mk_thm (to_term_list gs));
        []
    | 10 (* = kCompareLastTheorem *) ->
        let expected = Comms.receive_int() in
        let fingerprint = Theorem_fingerprint.fingerprint(fst (top_thm())) in
        (if fingerprint != expected then
          failwith ("Last theorem is not THM " ^ string_of_int expected ^
              " but " ^ string_of_int fingerprint ^
              "; Theorem: " ^ str_of_sexp (sexp_thm (fst (top_thm())))));
        []
    ) in
    Sys.set_signal Sys.sigint old_handler;
    result
  with e -> [kError;String (Printexc.to_string e)]

let () =
(* Indicate to parent process that initialization is complete. The value sent is
 * arbitrary and discarded by the parent. *)
Comms.send_int 0;
(* Ignore SIGINT while no request is being handled. *)
Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> ()));
while true do
  let response = handle_request() in
  List.iter (function
    | String s -> Comms.send_string s
    | Int n -> Comms.send_int n) response
done
