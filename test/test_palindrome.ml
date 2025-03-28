module To_test = struct
  let palindrome = Exercises.Palindrome.palindrome
end

let palindrome_test_cases =
  let check name expected input =
    Alcotest.(check bool) name expected (To_test.palindrome input)
  in
  let test_regular _ = check "regular" true [ 1; 2; 1 ] in
  let test_one _ = check "one" true [ 1 ] in
  let test_empty _ = check "empty" true [] in
  [
    Alcotest.test_case "regular" `Quick test_regular;
    Alcotest.test_case "empty" `Quick test_empty;
    Alcotest.test_case "one" `Quick test_one;
  ]

let () =
  let open Alcotest in
  run "Exercises" [ ("palindrome", palindrome_test_cases) ]
