(* This file must be included after the core. Otherwise we get an error "hd" *)

set_jrh_lexer;;

let DEBUG_MODE = ref false;;  (* Do not use this value directly, use is_debug_set *)

let debug_mode_env_set =
  try let _ = Sys.getenv "DEBUG_MODE" in true with _ -> false;;

Printexc.record_backtrace true;;

let enable_debug_mode() = DEBUG_MODE := debug_mode_env_set;;
let disable_debug_mode() = DEBUG_MODE := false;;

let DEBUG_MODE_COUNTER = ref 0;; (* Counts the number of times debug mode was needed *)
let is_debug_set s =
  if !DEBUG_MODE then
    (DEBUG_MODE_COUNTER := !DEBUG_MODE_COUNTER + 1;
    Printf.printf "DEBUG_MODE called for: %s; %!" s;
    Printf.printf "usages: %d \n%!" (!DEBUG_MODE_COUNTER);
    true)
  else false;;
