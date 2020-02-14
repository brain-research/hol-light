(* ========================================================================= *)
(* Simplistic HOL Light prettyprinter, using the OCaml "Format" library.     *)
(*                                                                           *)
(*       John Harrison, University of Cambridge Computer Laboratory          *)
(*                                                                           *)
(*            (c) Copyright, University of Cambridge 1998                    *)
(*              (c) Copyright, John Harrison 1998-2007                       *)
(*                  (c) Copyright, Google Inc. 2017                          *)
(* ========================================================================= *)

set_jrh_lexer;;
open System;;
open Lib;;
open Fusion;;
open Basics;;


(* ------------------------------------------------------------------------- *)
(* Character discrimination.                                                 *)
(* ------------------------------------------------------------------------- *)

let isspace,issep,isbra,issymb,isalpha,isnum,isalnum =
  let charcode s = Char.code(String.get s 0) in
  let spaces = " \t\n\r"
  and separators = ",;"
  and brackets = "()[]{}"
  and symbs = "\\!@#$%^&*-+|\\<=>/?~.:"
  and alphas = "'abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  and nums = "0123456789" in
  let allchars = spaces^separators^brackets^symbs^alphas^nums in
  let csetsize = itlist (max o charcode) (explode allchars) 256 in
  let ctable = Array.make csetsize 0 in
  do_list (fun c -> Array.set ctable (charcode c) 1) (explode spaces);
  do_list (fun c -> Array.set ctable (charcode c) 2) (explode separators);
  do_list (fun c -> Array.set ctable (charcode c) 4) (explode brackets);
  do_list (fun c -> Array.set ctable (charcode c) 8) (explode symbs);
  do_list (fun c -> Array.set ctable (charcode c) 16) (explode alphas);
  do_list (fun c -> Array.set ctable (charcode c) 32) (explode nums);
  let isspace c = Array.get ctable (charcode c) = 1
  and issep c  = Array.get ctable (charcode c) = 2
  and isbra c  = Array.get ctable (charcode c) = 4
  and issymb c = Array.get ctable (charcode c) = 8
  and isalpha c = Array.get ctable (charcode c) = 16
  and isnum c = Array.get ctable (charcode c) = 32
  and isalnum c = Array.get ctable (charcode c) >= 16 in
  isspace,issep,isbra,issymb,isalpha,isnum,isalnum;;

(* ------------------------------------------------------------------------- *)
(* Reserved words.                                                           *)
(* ------------------------------------------------------------------------- *)

let reserve_words,unreserve_words,is_reserved_word,reserved_words =
  let reswords = ref ["(";  ")"; "[";   "]";  "{";   "}";
                      ":";  ";";  ".";  "|";
                      "let"; "in"; "and"; "if"; "then"; "else";
                      "match"; "with"; "function"; "->"; "when"] in
  (fun ns  -> reswords := union (!reswords) ns),
  (fun ns  -> reswords := subtract (!reswords) ns),
  (fun n  -> mem n (!reswords)),
  (fun () -> !reswords);;

(* ------------------------------------------------------------------------- *)
(* Functions to access the global tables controlling special parse status.   *)
(*                                                                           *)
(*  o List of binders;                                                       *)
(*                                                                           *)
(*  o List of prefixes (right-associated unary functions like negation).     *)
(*                                                                           *)
(*  o List of infixes with their precedences and associations.               *)
(*                                                                           *)
(* Note that these tables are independent of constant/variable status or     *)
(* whether an identifier is symbolic.                                        *)
(* ------------------------------------------------------------------------- *)

let unparse_as_binder,parse_as_binder,parses_as_binder,binders =
  let binder_list = ref ([]:string list) in
  (fun n  -> binder_list := subtract (!binder_list) [n]),
  (fun n  -> binder_list := union (!binder_list) [n]),
  (fun n  -> mem n (!binder_list)),
  (fun () -> !binder_list);;

let unparse_as_prefix,parse_as_prefix,is_prefix,prefixes =
  let prefix_list = ref ([]:string list) in
  (fun n  -> prefix_list := subtract (!prefix_list) [n]),
  (fun n  -> prefix_list := union (!prefix_list) [n]),
  (fun n  -> mem n (!prefix_list)),
  (fun () -> !prefix_list);;

let unparse_as_infix,parse_as_infix,get_infix_status,infixes =
  let cmp (s,(x,a)) (t,(y,b)) =
     x < y || x = y && a > b || x = y && a = b && s < t in
  let infix_list = ref ([]:(string * (int * string)) list) in
  (fun n     -> infix_list := filter (((<>) n) o fst) (!infix_list)),
  (fun (n,d) -> infix_list := sort cmp
     ((n,d)::(filter (((<>) n) o fst) (!infix_list)))),
  (fun n     -> assoc n (!infix_list)),
  (fun ()    -> !infix_list);;

(* Helpful print methods for inspecting the infix_list. *)
let report_infix =
  (fun inf ->
    (report ((fst inf) ^ " "
    ^ (string_of_int (fst (snd inf))) ^ " "
    ^ (snd (snd inf)))))

let report_infixes =
  let infxs = infixes() in
  (fun () ->
    (map report_infix infxs));;

(* ------------------------------------------------------------------------- *)
(* Interface mapping.                                                        *)
(* ------------------------------------------------------------------------- *)

let the_interface = ref ([] :(string * (string * hol_type)) list);;

let the_overload_skeletons = ref ([] : (string * hol_type) list);;

(* ------------------------------------------------------------------------- *)
(* Now the printer.                                                          *)
(* ------------------------------------------------------------------------- *)

include Format;;

set_max_boxes 100;;

(* ------------------------------------------------------------------------- *)
(* Flag determining whether interface/overloading is reversed on printing.   *)
(* ------------------------------------------------------------------------- *)

let reverse_interface_mapping = ref true;;

(* ------------------------------------------------------------------------- *)
(* Determine binary operators that print without surrounding spaces.         *)
(* ------------------------------------------------------------------------- *)

let unspaced_binops = ref [","; ".."; "$"];;

(* ------------------------------------------------------------------------- *)
(* Binary operators to print at start of line when breaking.                 *)
(* ------------------------------------------------------------------------- *)

let prebroken_binops = ref ["==>"];;

(* ------------------------------------------------------------------------- *)
(* Force explicit indications of bound variables in set abstractions.        *)
(* ------------------------------------------------------------------------- *)

let print_unambiguous_comprehensions = ref false;;

(* ------------------------------------------------------------------------- *)
(* Print the universal set UNIV:A->bool as "(:A)".                           *)
(* ------------------------------------------------------------------------- *)

let typify_universal_set = ref true;;

(* ------------------------------------------------------------------------- *)
(* Flag controlling whether hypotheses print.                                *)
(* ------------------------------------------------------------------------- *)

let print_all_thm = ref true;;

(* ------------------------------------------------------------------------- *)
(* Get the name of a constant or variable.                                   *)
(* ------------------------------------------------------------------------- *)

let name_of tm =
  match tm with
    Var(x,ty) | Const(x,ty) -> x
  | _ -> "";;

(* ------------------------------------------------------------------------- *)
(* Printer for types.                                                        *)
(* ------------------------------------------------------------------------- *)

type sexp =
  | Sleaf of string
  | Snode of sexp list

let rec sexp_print fmt s = match s with
    Sleaf s -> pp_print_string fmt s
  | Snode [] -> pp_print_string fmt "()"
  | Snode (s::sl) ->
      pp_print_char fmt '(';
      sexp_print fmt s;
      List.iter (fun s -> pp_print_char fmt ' '; sexp_print fmt s) sl;
      pp_print_char fmt ')';;

let print_sexps (sexps : sexp list) (fmt : Format.formatter) : unit =
    List.iter (sexp_print fmt) sexps;
    pp_print_newline fmt ();;

(* Turn a converter raw : 'a -> sexp into a memoized converter memo : 'a -> sexp
   that writes either (memo _n raw) or _n, where _n is fresh. *)
let sexp_memoize : ('a -> sexp) -> ('a -> sexp) =
  let next = ref 1 in
  fun raw ->
    let memo = Hashtbl.create 100 in
    fun x ->
      try Sleaf (Hashtbl.find memo x)
      with Not_found ->
        let n = Printf.sprintf "_%d" (!next) in
        next := !next + 1;
        Hashtbl.add memo x n;
        Snode [Sleaf "memo"; Sleaf n; raw x]

let rec sexp_type ty = match ty with
    Tyvar str  -> Sleaf str
  | Tyapp (str,tyl) -> Snode (Sleaf str :: map sexp_type tyl)

let pp_print_type,pp_print_qtype =
  let soc sep flag ss =
    if ss = [] then "" else
    let s = end_itlist (fun s1 s2 -> s1^sep^s2) ss in
    if flag then "("^s^")" else s in
  let rec sot pr ty =
    try dest_vartype ty with Failure _ ->
    match dest_type ty with
      con,[] -> con
    | "fun",[ty1;ty2] -> soc "->" (pr > 0) [sot 1 ty1; sot 0 ty2]
    | "sum",[ty1;ty2] -> soc "+" (pr > 2) [sot 3 ty1; sot 2 ty2]
    | "prod",[ty1;ty2] -> soc "#" (pr > 4) [sot 5 ty1; sot 4 ty2]
    | "cart",[ty1;ty2] -> soc "^" (pr > 6) [sot 6 ty1; sot 7 ty2]
    | con,args -> (soc "," true (map (sot 0) args))^con in
  (fun fmt ty -> pp_print_string fmt (sot 0 ty)),
  (fun fmt ty -> pp_print_string fmt ("`:" ^ sot 0 ty ^ "`"));;

(* ------------------------------------------------------------------------- *)
(* Allow the installation of user printers. Must fail quickly if N/A.        *)
(* ------------------------------------------------------------------------- *)

let install_user_printer,delete_user_printer,try_user_printer =
  let user_printers = ref ([]:(string*(formatter->term->unit))list) in
  (fun pr -> user_printers := pr::(!user_printers)),
  (fun s -> user_printers := snd(remove (fun (s',_) -> s = s')
                                        (!user_printers))),
  (fun fmt -> fun tm -> tryfind (fun (_,pr) -> pr fmt tm) (!user_printers));;

(* ------------------------------------------------------------------------- *)
(* Printer for terms.                                                        *)
(* ------------------------------------------------------------------------- *)

let pp_print_term =
  let reverse_interface (s0,ty0) =
    if not(!reverse_interface_mapping) then s0 else
    try fst(find (fun (s,(s',ty)) -> s' = s0 && can (type_match ty ty0) [])
                 (!the_interface))
    with Failure _ -> s0 in
  let DEST_BINARY c tm =
    try let il,r = dest_comb tm in
        let i,l = dest_comb il in
        if i = c ||
           (is_const i && is_const c &
            reverse_interface(dest_const i) = reverse_interface(dest_const c))
        then l,r else fail()
    with Failure _ -> failwith "DEST_BINARY"
  and ARIGHT s =
    match snd(get_infix_status s) with
    "right" -> true | _ -> false in
  let rec powerof10 n =
    if abs_num n </ Int 1 then false
    else if n =/ Int 1 then true
    else powerof10 (n // Int 10) in
  let bool_of_term t =
    match t with
      Const("T",_) -> true
    | Const("F",_) -> false
    | _ -> failwith "bool_of_term" in
  let code_of_term t =
    let f,tms = strip_comb t in
    if not(is_const f && fst(dest_const f) = "ASCII")
       || not(length tms = 8) then failwith "code_of_term"
    else
       itlist (fun b f -> if b then 1 + 2 * f else 2 * f)
              (map bool_of_term (rev tms)) 0 in
  let rec dest_clause tm =
    let pbod = snd(strip_exists(body(body tm))) in
    let s,args = strip_comb pbod in
    if name_of s = "_UNGUARDED_PATTERN" && length args = 2 then
      [rand(rator(hd args));rand(rator(hd(tl args)))]
    else if name_of s = "_GUARDED_PATTERN" && length args = 3 then
      [rand(rator(hd args)); hd(tl args); rand(rator(hd(tl(tl args))))]
    else failwith "dest_clause" in
  let rec dest_clauses tm =
    let s,args = strip_comb tm in
    if name_of s = "_SEQPATTERN" && length args = 2 then
      dest_clause (hd args)::dest_clauses(hd(tl args))
    else [dest_clause tm] in

  fun fmt ->
    let rec print_term prec tm =
      try try_user_printer fmt tm with Failure _ ->
      try pp_print_string fmt (string_of_num(dest_numeral tm)) with Failure _ ->
      try (let tms = dest_list tm in
           try if fst(dest_type(hd(snd(dest_type(type_of tm))))) <> "char"
               then fail() else
               let ccs = map (String.make 1 o Char.chr o code_of_term) tms in
               let s = "\"" ^ String.escaped (implode ccs) ^ "\"" in
               pp_print_string fmt s
           with Failure _ ->
               pp_print_string fmt "[";
               print_term_sequence "; " 0 tms;
               pp_print_string fmt "]")
      with Failure _ ->
      if is_gabs tm then print_binder prec tm else
      let hop,args = strip_comb tm in
      let s0 = name_of hop
      and ty0 = type_of hop in
      let s = reverse_interface (s0,ty0) in
      try if s = "EMPTY" && is_const tm && args = [] then
          pp_print_string fmt "{}" else fail()
      with Failure _ ->
      try if s = "UNIV" && !typify_universal_set && is_const tm && args = [] then
            let ty = fst(dest_fun_ty(type_of tm)) in
            (pp_print_string fmt "(:";
             pp_print_type fmt ty;
             pp_print_string fmt ")")
          else fail()
      with Failure _ ->
      try if s <> "INSERT" then fail() else
          let mems,oth = splitlist (dest_binary "INSERT") tm in
          if is_const oth && fst(dest_const oth) = "EMPTY" then
            (pp_print_string fmt "{";
             print_term_sequence ", " 14 mems;
             pp_print_string fmt "}")
          else fail()
      with Failure _ ->
      try if not (s = "GSPEC") then fail() else
          let evs,bod = strip_exists(body(rand tm)) in
          let bod1,fabs = dest_comb bod in
          let bod2,babs = dest_comb bod1 in
          let c = rator bod2 in
          if fst(dest_const c) <> "SETSPEC" then fail() else
          pp_print_string fmt "{";
          print_term 0 fabs;
          pp_print_string fmt " | ";
          (let fvs = frees fabs and bvs = frees babs in
           if not(!print_unambiguous_comprehensions) &
              set_eq evs
               (if (length fvs <= 1 || bvs = []) then fvs
                else intersect fvs bvs)
           then ()
           else (print_term_sequence "," 14 evs;
                 pp_print_string fmt " | "));
          print_term 0 babs;
          pp_print_string fmt "}"
      with Failure _ ->
      try let eqs,bod = dest_let tm in
          (if prec = 0 then pp_open_hvbox fmt 0
           else (pp_open_hvbox fmt 1; pp_print_string fmt "(");
           pp_print_string fmt "let ";
           print_term 0 (mk_eq(hd eqs));
           do_list (fun (v,t) -> pp_print_break fmt 1 0;
                                 pp_print_string fmt "and ";
                                 print_term 0 (mk_eq(v,t)))
                   (tl eqs);
           pp_print_string fmt " in";
           pp_print_break fmt 1 0;
           print_term 0 bod;
           if prec = 0 then () else pp_print_string fmt ")";
           pp_close_box fmt ())
      with Failure _ -> try
        if s <> "DECIMAL" then fail() else
        let n_num = dest_numeral (hd args)
        and n_den = dest_numeral (hd(tl args)) in
        if not(powerof10 n_den) then fail() else
        let s_num = string_of_num(quo_num n_num n_den) in
        let s_den = implode(tl(explode(string_of_num
                        (n_den +/ (mod_num n_num n_den))))) in
        pp_print_string fmt("#"^s_num^(if n_den = Int 1 then "" else ".")^s_den)
      with Failure _ -> try
        if s <> "_MATCH" || length args <> 2 then failwith "" else
        let cls = dest_clauses(hd(tl args)) in
        (if prec = 0 then () else pp_print_string fmt "(";
         pp_open_hvbox fmt 0;
         pp_print_string fmt "match ";
         print_term 0 (hd args);
         pp_print_string fmt " with";
         pp_print_break fmt 1 2;
         print_clauses cls;
         pp_close_box fmt ();
         if prec = 0 then () else pp_print_string fmt ")")
      with Failure _ -> try
        if s <> "_FUNCTION" || length args <> 1 then failwith "" else
        let cls = dest_clauses(hd args) in
        (if prec = 0 then () else pp_print_string fmt "(";
         pp_open_hvbox fmt 0;
         pp_print_string fmt "function";
         pp_print_break fmt 1 2;
         print_clauses cls;
         pp_close_box fmt ();
         if prec = 0 then () else pp_print_string fmt ")")
      with Failure _ ->
      if s = "COND" && length args = 3 then
        (if prec = 0 then () else pp_print_string fmt "(";
         pp_open_hvbox fmt (-1);
         pp_print_string fmt "if ";
         print_term 0 (hd args);
         pp_print_break fmt 0 0;
         pp_print_string fmt " then ";
         print_term 0 (hd(tl args));
         pp_print_break fmt 0 0;
         pp_print_string fmt " else ";
         print_term 0 (hd(tl(tl args)));
         pp_close_box fmt ();
         if prec = 0 then () else pp_print_string fmt ")")
      else if is_prefix s && length args = 1 then
        (if prec = 1000 then pp_print_string fmt "(" else ();
         pp_print_string fmt s;
         (if isalnum s ||
           s = "--" &
           length args = 1 &
           (try let l,r = dest_comb(hd args) in
                let s0 = name_of l and ty0 = type_of l in
                reverse_interface (s0,ty0) = "--" ||
                mem (fst(dest_const l)) ["real_of_num"; "int_of_num"]
            with Failure _ -> false) ||
           s = "~" && length args = 1 && is_neg(hd args)
          then pp_print_string fmt " " else ());
         print_term 999 (hd args);
         if prec = 1000 then pp_print_string fmt ")" else ())
      else if parses_as_binder s && length args = 1 && is_gabs (hd args) then
        print_binder prec tm
      else if can get_infix_status s && length args = 2 then
        let bargs =
          if ARIGHT s then
            let tms,tmt = splitlist (DEST_BINARY hop) tm in tms@[tmt]
          else
            let tmt,tms = rev_splitlist (DEST_BINARY hop) tm in tmt::tms in
        let newprec = fst(get_infix_status s) in
        (if newprec <= prec then
          (pp_open_hvbox fmt 1; pp_print_string fmt "(")
         else pp_open_hvbox fmt 0;
         print_term newprec (hd bargs);
         do_list (fun x -> if mem s (!unspaced_binops) then ()
                           else if mem s (!prebroken_binops)
                           then pp_print_break fmt 1 0
                           else pp_print_string fmt " ";
                           pp_print_string fmt s;
                           if mem s (!unspaced_binops)
                           then pp_print_break fmt 0 0
                           else if mem s (!prebroken_binops)
                           then pp_print_string fmt " "
                           else pp_print_break fmt 1 0;
                           print_term newprec x) (tl bargs);
         if newprec <= prec then pp_print_string fmt ")" else ();
         pp_close_box fmt ())
      else if (is_const hop || is_var hop) && args = [] then
        let s' = if parses_as_binder s || can get_infix_status s || is_prefix s
                 then "("^s^")" else s in
        pp_print_string fmt s'
      else
        let l,r = dest_comb tm in
        (pp_open_hvbox fmt 0;
         if prec = 1000 then pp_print_string fmt "(" else ();
         print_term 999 l;
         (if try mem (fst(dest_const l)) ["real_of_num"; "int_of_num"]
             with Failure _ -> false
          then () else pp_print_space fmt ());
         print_term 1000 r;
         if prec = 1000 then pp_print_string fmt ")" else ();
         pp_close_box fmt ())

    and print_term_sequence sep prec tms =
      if tms = [] then () else
      (print_term prec (hd tms);
       let ttms = tl tms in
       if ttms = [] then ()
       else (pp_print_string fmt sep; print_term_sequence sep prec ttms))

    and print_binder prec tm =
      let absf = is_gabs tm in
      let s = if absf then "\\" else name_of(rator tm) in
      let rec collectvs tm =
        if absf then
          if is_abs tm then
            let v,t = dest_abs tm in
            let vs,bod = collectvs t in (false,v)::vs,bod
          else if is_gabs tm then
            let v,t = dest_gabs tm in
            let vs,bod = collectvs t in (true,v)::vs,bod
          else [],tm
        else if is_comb tm && name_of(rator tm) = s then
          if is_abs(rand tm) then
            let v,t = dest_abs(rand tm) in
            let vs,bod = collectvs t in (false,v)::vs,bod
          else if is_gabs(rand tm) then
            let v,t = dest_gabs(rand tm) in
            let vs,bod = collectvs t in (true,v)::vs,bod
          else [],tm
        else [],tm in
      let vs,bod = collectvs tm in
      ((if prec = 0 then pp_open_hvbox fmt 4
        else (pp_open_hvbox fmt 5; pp_print_string fmt "("));
       pp_print_string fmt s;
       (if isalnum s then pp_print_string fmt " " else ());
       do_list (fun (b,x) ->
         (if b then pp_print_string fmt "(" else ());
         print_term 0 x;
         (if b then pp_print_string fmt ")" else ());
         pp_print_string fmt " ") (butlast vs);
       (if fst(last vs) then pp_print_string fmt "(" else ());
       print_term 0 (snd(last vs));
       (if fst(last vs) then pp_print_string fmt ")" else ());
       pp_print_string fmt ".";
       (if length vs = 1 then pp_print_string fmt " "
        else pp_print_space fmt ());
       print_term 0 bod;
       (if prec = 0 then () else pp_print_string fmt ")");
       pp_close_box fmt ())

    and print_clauses cls =
      match cls with
        [c] -> print_clause c
      | c::cs -> (print_clause c;
                  pp_print_break fmt 1 0;
                  pp_print_string fmt "| ";
                  print_clauses cs)

    and print_clause cl =
      match cl with
        [p;g;r] -> (print_term 1 p;
                    pp_print_string fmt " when ";
                    print_term 1 g;
                    pp_print_string fmt " -> ";
                    print_term 1 r)
      | [p;r] -> (print_term 1 p;
                  pp_print_string fmt " -> ";
                  print_term 1 r)

    in print_term 0;;

(* ------------------------------------------------------------------------- *)
(* Print term with quotes.                                                   *)
(* ------------------------------------------------------------------------- *)

let pp_print_qterm fmt tm =
  pp_print_string fmt "`";
  pp_print_term fmt tm;
  pp_print_string fmt "`";;

(* ------------------------------------------------------------------------- *)
(* Printer for theorems.                                                     *)
(* ------------------------------------------------------------------------- *)

let pp_print_thm fmt th =
  let asl,tm = dest_thm th in
  (if not (asl = []) then
    (if !print_all_thm then
      (pp_print_term fmt (hd asl);
       do_list (fun x -> pp_print_string fmt ",";
                         pp_print_space fmt ();
                         pp_print_term fmt x)
               (tl asl))
     else pp_print_string fmt "...";
     pp_print_space fmt ())
   else ();
   pp_open_hbox fmt();
   pp_print_string fmt "|- ";
   pp_print_term fmt tm;
   pp_close_box fmt ());;

(* ------------------------------------------------------------------------- *)
(* A printer for goals etc.                                                  *)
(* ------------------------------------------------------------------------- *)

let pp_print_goal fmt =
  let string_of_int3 n =
    if n < 10 then "  "^string_of_int n
    else if n < 100 then " "^string_of_int n
    else string_of_int n in
  let print_hyp n (s,th) =
    Format.pp_open_hbox fmt ();
    Format.pp_print_string fmt (string_of_int3 n);
    Format.pp_print_string fmt " [";
    Format.pp_open_hvbox fmt 0;
    pp_print_term fmt (concl th);
    Format.pp_close_box fmt ();
    Format.pp_print_string fmt "]";
    (if not (s = "") then (Format.pp_print_string fmt (" ("^s^")")) else ());
    Format.pp_close_box fmt ();
    Format.pp_print_newline fmt () in
  let rec print_hyps n asl =
    if asl = [] then () else
    (print_hyp n (hd asl);
     print_hyps (n + 1) (tl asl)) in
  fun (asl,w) ->
    Format.pp_print_newline fmt ();
    if asl <> [] then (print_hyps 0 (rev asl); Format.pp_print_newline fmt ()) else ();
    pp_print_qterm fmt w; Format.pp_print_newline fmt ();;

(* ------------------------------------------------------------------------- *)
(* Print on standard output.                                                 *)
(* ------------------------------------------------------------------------- *)

let print_type = pp_print_type std_formatter;;
let print_qtype = pp_print_qtype std_formatter;;
let print_term = pp_print_term std_formatter;;
let print_qterm = pp_print_qterm std_formatter;;
let print_thm = pp_print_thm std_formatter;;

(* ------------------------------------------------------------------------- *)
(* Install all the printers.                                                 *)
(* ------------------------------------------------------------------------- *)

(* Disabled for native build: #install_printer print_qtype;; *)
(* Disabled for native build: #install_printer print_qterm;; *)
(* Disabled for native build: #install_printer print_thm;; *)

(* ------------------------------------------------------------------------- *)
(* Conversions to string.                                                    *)
(* ------------------------------------------------------------------------- *)

let print_to_string printer =
  let buf = Buffer.create 16 in
  let fmt = formatter_of_buffer buf in
  let () = pp_set_max_boxes fmt 100 in
  let print = printer fmt in
  let flush = pp_print_flush fmt in
  fun x ->
    let () = pp_set_margin fmt (get_margin ()) in
    let () = print x in
    let () = flush () in
    let s = Buffer.contents buf in
    let () = Buffer.reset buf in
    s;;

let string_of_type = print_to_string pp_print_type;;
let string_of_term = print_to_string pp_print_term;;
let string_of_thm = print_to_string pp_print_thm;;
let string_of_goal = print_to_string pp_print_goal;;

(* ------------------------------------------------------------------------- *)
(* Parseable S-Expression printers                                           *)
(* ------------------------------------------------------------------------- *)

let rec sexp_term tm =
  if false then Sleaf ("`" ^ string_of_term tm ^ "`") else  (* For debugging purposes *)
  match tm with
    Var (str, ty) -> Snode [Sleaf "v"; sexp_type ty; Sleaf str]
  | Const (str, ty) -> Snode [Sleaf "c"; sexp_type ty; Sleaf str]
  | Comb (t1, t2) -> Snode [Sleaf "a"; sexp_term t1; sexp_term t2]
  | Abs (t1, t2) -> Snode [Sleaf "l"; sexp_term t1; sexp_term t2];;


let sexp_thm =
  (* don't memoize (set condition to true) when generating training data *)
  if true then (fun th ->
    let tls, tm = dest_thm th in
    Snode [Sleaf "h"; Snode (map sexp_term tls); sexp_term tm]) else
  sexp_memoize (fun th ->
      let tls, tm = dest_thm th in
      Snode [Sleaf "h"; Snode (map sexp_term tls); sexp_term tm]);;

let str_of_sexp s = sexp_print Format.str_formatter s; flush_str_formatter();;

type term_encoding = Pretty | Sexp
let current_encoding = ref Pretty
let encode_term t : string = match !current_encoding with
    Pretty -> string_of_term t
  | Sexp -> str_of_sexp (sexp_term t);;


(* ------------------------------------------------------------------------- *)
(* Formatters for proof and theorem recording                                *)
(* Each formatter writes to a file that is specified in an env variable.     *)
(* ------------------------------------------------------------------------- *)

let formatter_from_environment_variable (env_var : string)
                                        (file_extension : string)
                                        : Format.formatter option =
  try
    let filename = Sys.getenv env_var in
    let proof_log_oc = open_out (filename ^ file_extension) in
    Some Format.formatter_of_out_channel proof_log_oc
  with Not_found -> None;;

let prooflog_pb_fmt : Format.formatter option =
  formatter_from_environment_variable "PROOFLOG_PB_OUTPUT" "";;

let thm_db_fmt : Format.formatter option =
  formatter_from_environment_variable "THM_DB_OUTPUT" "";;
