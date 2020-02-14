set_jrh_lexer;;
open Test_common;;
open Printf;;

let total_tests = List.length !test_cases
let failed_tests = ref 0
let successful_tests = ref 0

let run_test (name, test) =
  try
    test ();
    incr successful_tests;
    printf "Test %s ran successfully.\n%!" name;
  with Failure failtext ->
    incr failed_tests;
    printf "Test %s FAILED: %s\n%!" name failtext;;

printf "Running %d tests\n%!" total_tests;;
List.iter run_test !test_cases;;
printf "\nTests succeeded: %d\nTests failed: %d\n%!"
  !successful_tests !failed_tests;;

