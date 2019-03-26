
set_jrh_lexer;;

open Lib;;
open Printf;;
open Fusion;;
open Printer;;
open Log;;
open Import_proofs;;

(* This file prints a summary after importing proofs from DeepHOL *)

printf "Goal fingerprints (without core) that weren't checked: \n";;
if Hashtbl.length has_to_be_checked == 0
then printf "  None. Yay!"
else Hashtbl.iter (fun fp -> fun () -> printf "  %d\n" fp) has_to_be_checked;;
printf "\n\n%!";;

printf "Number of proofs checked: %d\n%!" (!num_proofs_replaced);;
printf "Number of proofs succeeded: %d\n%!" (!num_replaced_proofs_succeeded);;
printf "Number of proofs failed: %d\n%!" (!num_replaced_proofs_failed);;
