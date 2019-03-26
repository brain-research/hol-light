set_jrh_lexer;;
open Lib;;
open Printer;;

Printexc.record_backtrace true;;  (* enables stacktraces *)

let test_cases : (string * (unit -> unit)) list ref = ref [];;

let register_test name test arg =
  test_cases := (name, fun () -> test arg) :: !test_cases;;

let register_tests name test args =
  List.iteri
    (fun idx arg -> register_test (Printf.sprintf "%s %d" name idx) test arg)
    args;;

let assert_fail e x =
  (try e x
  with _ -> ());
  failwith "Assertion failed: expected exception.";;

let assert_equal (x: string) (y: string) =
  if String.equal x y then ()
  else failwith (sprintf "Assertion failed: '%s' not equal to '%s'" x y);;

let assert_equal_generic (fmt: 'a -> string) (x: 'a) (y: 'a) =
  let x, y = fmt x, fmt y in
  assert_equal x y;;

let assert_equal_terms tm1 tm2 =
  assert_equal_generic (str_of_sexp o sexp_term) tm1 tm2;;

let assert_equal_theorems th1 th2 =
  assert_equal_generic (str_of_sexp o sexp_thm) th1 th2;;

let assert_idempotent (assert_equal: 'a -> 'a -> unit) (f: 'a -> 'a) (x: 'a) =
  assert_equal (f x) (f (f x));;
